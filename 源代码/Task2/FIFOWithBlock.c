//------------------------------------------------------------------------
#include <linux/init.h>
#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/cdev.h>
#include <linux/fs.h>
#include <linux/errno.h>
#include <asm/current.h>
#include <linux/sched.h>
#include <linux/uaccess.h>
#include <asm/atomic.h>
#include <linux/mutex.h>
#include <linux/wait.h>
#include <linux/device.h>
#include <linux/kfifo.h>
#include <linux/miscdevice.h>

#include <linux/slab.h>
#include <linux/gfp.h>
#include <linux/poll.h>

#include <linux/wait.h>

#define DEV_NAME "FIFOWithBlock"
#define BUFFER_SIZE 32
#define MISC_MINOR  114
//------------------------------------------------------------------------

// define Kernel FIFO Ring Buffer
DEFINE_KFIFO(FIFOBuffer, char, BUFFER_SIZE);
struct mutex mutex_r, mutex_w, can_read, can_write;

struct _BlockDevice{
	const char *name;
	struct device *device;
	struct miscdevice *miscdev;
	wait_queue_head_t read_queue;
	wait_queue_head_t write_queue;
};
struct _BlockDevice * BlockDevice;
struct _BlockDevice temp;

int ret, actual_readed, actul_write;



//-operations-----------------------------------------------------------------------
// open device
static int FIFOWithBlock_open(struct inode *inode, struct file *filp) {
	printk(KERN_EMERG "open device\n");
	return 0;
}

// release device
static int FIFOWithBlock_release(struct inode * inode, struct file * filp){
	printk(KERN_EMERG "release\n");
	return 0;
}




static ssize_t FIFOWithBlock_read(struct file *file, char __user *buf, size_t count, loff_t *offset) {

	
	//读判断条件的锁
	if (file->f_flags & O_NONBLOCK) {
		if (mutex_trylock(&mutex_r) != 1)//非阻塞方式无法进入条件的判断而直接返回
		{
			printk(KERN_EMERG "[INFO]: lock in NONBLOCK FAILED\n");
			return -EAGAIN;
		}
	}
	else mutex_lock(&mutex_r); //阻塞方式进行等待读判断锁
	
	
	// 进行缓冲区的判别
	if (kfifo_is_empty(&FIFOBuffer)) {// buffer is empty, no data to read
		mutex_unlock(&mutex_r);// 释放条件判断锁
		if (file->f_flags & O_NONBLOCK) {
			printk(KERN_EMERG "[INFO]: Buffer is EMPTY, read Failed\n"); //非阻塞方式无数据可读直接返回
			return -EAGAIN;
		}
		
		ret = wait_event_interruptible(BlockDevice->read_queue, ((!kfifo_is_empty(&FIFOBuffer)) && (mutex_trylock(&can_read) == 1)));

	}
	else mutex_unlock(&mutex_r);// 释放条件判断锁
	


	if (!mutex_is_locked(&can_read)) mutex_lock(&can_read); //这里会不会也会阻塞非阻塞模式呢
	// read
	if (kfifo_to_user(&FIFOBuffer, buf, count, &actual_readed) == 0) {
		printk(KERN_EMERG "[INFO]: Read %d bytes successfully. Used buffer size: %d \n", actual_readed, kfifo_len(&FIFOBuffer));
	}
		
	mutex_unlock(&can_read); //释放读锁
		
	
	if(!kfifo_is_full(&FIFOBuffer)){
		wake_up_interruptible(&BlockDevice->write_queue);
	}
	
	return actual_readed;
}


static ssize_t FIFOWithBlock_write(struct file *file, const char __user *buf, size_t count, loff_t *offset) {
	
	
	if (file->f_flags & O_NONBLOCK) {
		if (mutex_trylock(&mutex_w) != 1) //非阻塞模式无法获取判断条件，那么直接返回
		{
			printk(KERN_EMERG "[INFO]: lock in NONBLOCK FAILED\n");
			return -EAGAIN;
		}
	}
	else mutex_lock(&mutex_w); //阻塞模式等待判断条件的锁
	
	
	// 缓冲区满无法写
	if (kfifo_is_full(&FIFOBuffer)) {
		mutex_unlock(&mutex_w);// 释放条件判断锁
		if (file->f_flags & O_NONBLOCK) {
			printk(KERN_EMERG "[INFO]: Buffer is FULL, write Failed\n");
			return -EAGAIN;
		}
		// 将阻塞模式的加入等待队列
		ret = wait_event_interruptible(BlockDevice->write_queue, ((!kfifo_is_empty(&FIFOBuffer)) && (mutex_trylock(&can_read) == 1)));

	}
	else mutex_unlock(&mutex_w);// 释放条件判断锁


	// 申请写锁
	if (!mutex_is_locked(&can_write)) mutex_lock(&can_write);

	// 写入内容
	if (kfifo_from_user(&FIFOBuffer, buf, count, &actul_write) == 0) {
		printk(KERN_EMERG "[INFO]: write %d bytes successfully. Used buffer size: %d \n", actul_write, kfifo_len(&FIFOBuffer));
	}
		
	mutex_unlock(&can_write);
		
	// 唤醒等待队列
	if(!kfifo_is_full(&FIFOBuffer)){
		wake_up_interruptible(&BlockDevice->read_queue);
	}
	
	return actul_write;
}


// file struct
static const struct file_operations fops = {
	.owner = THIS_MODULE,
	.open = FIFOWithBlock_open,
	.release = FIFOWithBlock_release,
	.read = FIFOWithBlock_read,
	.write = FIFOWithBlock_write,
};

static struct miscdevice miscDeviceFIFOBlock = {
	.minor = MISC_MINOR,
	.name = DEV_NAME,
	.fops = &fops,
};

// init
static int __init FIFOWithBlock_init(void){
	ret = misc_register(&miscDeviceFIFOBlock);
	BlockDevice = &temp;
//	BlockDevice = kmalloc(sizeof(struct _BlockDevice), GFP_KERNEL);
	BlockDevice->miscdev = &miscDeviceFIFOBlock;
	init_waitqueue_head(&BlockDevice->read_queue);
	init_waitqueue_head(&BlockDevice->write_queue);
		
	mutex_unlock(&can_read);
	mutex_unlock(&can_write);
	mutex_unlock(&mutex_r);
	mutex_unlock(&mutex_w);
	return 0;
}

// exit
static void  __exit FIFOWithBlock_exit(void){
	misc_deregister(&miscDeviceFIFOBlock);
}

module_init(FIFOWithBlock_init);
module_exit(FIFOWithBlock_exit);
MODULE_LICENSE("GPL");

