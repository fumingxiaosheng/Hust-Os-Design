
BASEINCLUDE ?= /lib/modules/55.13.0-30-generic/build

DevFIFOWithBlock-objs := FIFOWithBlock.o
obj-m := DevFIFOWithBlock.o

CURRENT_PATH :=$(shell pwd)
VERSION_NUM :=$(shell uname -r)
LIUNX_PATH :=/usr/src/linux-headers-$(VERSION_NUM)

all:
	make -C $(LIUNX_PATH) M=$(CURRENT_PATH) modules
clean:
	make -C $(LIUNX_PATH) M=$(CURRENT_PATH) clean

