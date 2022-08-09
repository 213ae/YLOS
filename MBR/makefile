.PHONY : all clean rebuild

BOOT_SRC := boot.asm
BOOT_OUT := boot.bin
LOADER_SRC := loader.asm
LOADER_OUT := loader.bin
IMG := data.img
DEBUG := debug.txt
LOADER_PATH := /mnt/fd

RM := rm -fr 

all : $(IMG) $(BOOT_OUT) $(LOADER_OUT)
	dd if=$(BOOT_OUT) of=$(IMG) bs=512 count=1 conv=notrunc
	sudo mount -o loop $(IMG) $(LOADER_PATH)
	sudo cp $(LOADER_OUT) $(LOADER_PATH)
	sudo umount $(LOADER_PATH)
	@echo "Success!"

$(DEBUG) : $(BOOT_OUT)
	ndisasm -o 0x7c00 $^ > $@

%.bin : %.asm
	nasm $^ -o $@

$(IMG) : 
	bximage $@ -q -fd -size=1.44

clean : 
	$(RM) $(IMG) $(BOOT_OUT) $(LOADER_OUT) 

rebuild : clean all
		
