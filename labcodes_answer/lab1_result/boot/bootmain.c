#include <defs.h>
#include <x86.h>
#include <elf.h>

/* *********************************************************************
 * This a dirt simple boot loader, whose sole job is to boot
 * an ELF kernel image from the first IDE hard disk.
 *
 * DISK LAYOUT
 *  * This program(bootasm.S and bootmain.c) is the bootloader.
 *    It should be stored in the first sector of the disk.
 *
 *  * The 2nd sector onward holds the kernel image.
 *
 *  * The kernel image must be in ELF format.
 *
 * BOOT UP STEPS
 *  * when the CPU boots it loads the BIOS into memory and executes it
 *
 *  * the BIOS intializes devices, sets of the interrupt routines, and
 *    reads the first sector of the boot device(e.g., hard-drive)
 *    into memory and jumps to it.
 *
 *  * Assuming this boot loader is stored in the first sector of the
 *    hard-drive, this code takes over...
 *
 *  * control starts in bootasm.S -- which sets up protected mode,
 *    and a stack so C code then run, then calls bootmain()
 *
 *  * bootmain() in this file takes over, reads in the kernel and jumps to it.
 * */

/*
kernel elf文件的头看一下就知道为啥要这样写了的
ELF Header:
  Magic:   7f 45 4c 46 01 01 01 00 00 00 00 00 00 00 00 00
  Class:                             ELF32
  Data:                              2's complement, little endian
  Version:                           1 (current)
  OS/ABI:                            UNIX - System V
  ABI Version:                       0
  Type:                              EXEC (Executable file)
  Machine:                           Intel 80386
  Version:                           0x1
  Entry point address:               0x100000
  Start of program headers:          52 (bytes into file)
  Start of section headers:          74428 (bytes into file)
  Flags:                             0x0
  Size of this header:               52 (bytes)
  Size of program headers:           32 (bytes)
  Number of program headers:         3
  Size of section headers:           40 (bytes)
  Number of section headers:         11
  Section header string table index: 10

Elf file type is EXEC (Executable file)
Entry point 0x100000
There are 3 program headers, starting at offset 52

//virtual这个是程序的虚拟地址，也叫做逻辑地址
Program Headers:
  Type           Offset   VirtAddr   PhysAddr   FileSiz MemSiz  Flg Align
  LOAD           0x001000 0x00100000 0x00100000 0x0dd19 0x0dd19 R E 0x1000  4KB对齐
  LOAD           0x00f000 0x0010e000 0x0010e000 0x00a16 0x01d80 RW  0x1000  4KB对齐
  GNU_STACK      0x000000 0x00000000 0x00000000 0x00000 0x00000 RWE 0x10

 Section to Segment mapping:
  Segment Sections...
   00     .text .rodata .stab .stabstr
   01     .data .bss
   02

gdt:
    SEG_NULLASM                                     # null seg，第一个是空的
    # 代码段，可以读可以执行，段的基址0，limit很大是4G
    SEG_ASM(STA_X|STA_R, 0x0, 0xffffffff)           # code seg for bootloader and kernel
    # 数据段，有写权限
    SEG_ASM(STA_W, 0x0, 0xffffffff)                 # data seg for bootloader and kernel
*/

unsigned int    SECTSIZE  =      512;  //一个扇区的字节数是512
struct elfhdr * ELFHDR    =      ((struct elfhdr *)0x10000); //scratch space，elf的entry point是0x100000

/* waitdisk - wait for disk ready */
static void
waitdisk(void) {
    while ((inb(0x1F7) & 0xC0) != 0x40)
        /* do nothing */;
}

/* readsect - read a single sector at @secno into @dst */
static void
readsect(void *dst, uint32_t secno) {
    // wait for disk to be ready
    //dst=0x10000,secno=1
    waitdisk();

    outb(0x1F2, 1);                         // count = 1
    outb(0x1F3, secno & 0xFF);
    outb(0x1F4, (secno >> 8) & 0xFF);
    outb(0x1F5, (secno >> 16) & 0xFF);
    outb(0x1F6, ((secno >> 24) & 0xF) | 0xE0);
    outb(0x1F7, 0x20);                      // cmd 0x20 - read sectors

    // wait for disk to be ready
    waitdisk();

    // read a sector
    insl(0x1F0, dst, SECTSIZE / 4);
}

/* *
 * readseg - read @count bytes at @offset from kernel into virtual address @va,
 * might copy more than asked.
 * */
static void
readseg(uintptr_t va, uint32_t count, uint32_t offset) {
    //va is virtual address
    //offset 相对于kernel elf文件起始位置的偏移
    uintptr_t end_va = va + count;

    // round down to sector boundary
    // 四舍五入 %就是在取余数
    va -= offset % SECTSIZE;  //目前载入的两个segment都是4KB对齐的，摸都是0，va都是不变的

    // translate from bytes to sectors; kernel starts at sector 1
    uint32_t secno = (offset / SECTSIZE) + 1; //+1是由于第一个sector是BootLoader，kernel的ELF文件在第二个扇区开始的位置

    // If this is too slow, we could read lots of sectors at a time.
    // We'd write more to memory than asked, but it doesn't matter --
    // we load in increasing order.
    for (; va < end_va; va += SECTSIZE, secno ++) {
        readsect((void *)va, secno);  //循环的去读8个sector的数据
    }
}

/* bootmain - the entry of bootloader */
void
bootmain(void) {
    // read the 1st page off disk
    //readseg(0x100000, 512 * 8, 0);
    readseg((uintptr_t)ELFHDR, SECTSIZE * 8, 0);

    // is this a valid ELF?
    if (ELFHDR->e_magic != ELF_MAGIC) {
        goto bad;
    }

    struct proghdr *ph, *eph;  //program header

    // load each program segment (ignores ph flags)
    // elf文件的装载视图的按段整合之后有一个描述符表叫program header table
    // 其实就是一个数组，按照顺序排列的所有segment的描述符，也叫元数据
    ph = (struct proghdr *)((uintptr_t)ELFHDR + ELFHDR->e_phoff);  //pragram headers的地址
    eph = ph + ELFHDR->e_phnum;  //program entries的数量
    for (; ph < eph; ph ++) {
        //ph->p_va是map当前段的虚拟地址
        //ph->p_memsz 该segment的大小
        //ph->p_offset 在elf文件中的偏移
        readseg(ph->p_va & 0xFFFFFF, ph->p_memsz, ph->p_offset);
    }
    // kernel的段会被加载到内核的线性地址空间中
    // ELF文件0x1000位置后面的0x0dd19比特被载入内存0x00100000
    // ELF文件0xf000位置后面的0x01d80比特被载入内存0x0010e000
    // 目前为止这些地址都是线性地址空间，在段机制的情况下，这些均为物理地址，所以现在虽然是有映射的，但是实际上还是类似实模式的

    // call the entry point from the ELF header
    // note: does not return
    ((void (*)(void))(ELFHDR->e_entry & 0xFFFFFF))();  //kernel的入口了就是

bad:
    outw(0x8A00, 0x8A00);
    outw(0x8A00, 0x8E00);

    /* do nothing */
    while (1);
}

