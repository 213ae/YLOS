org 0x7c00

jmp short start
nop

define:
    BaseOfStack equ 0x7c00
    BaseOfLoader equ 0x9000
    EntryLength equ 32
    FielNameLength equ 11
    RootDirSec equ 19
    RootDirSize equ 14
    FatTableSec equ 1
    FatTableSize equ 9

header:
    BS_OEMName     db "YLOS    "
    BPB_BytsPerSec dw 512
    BPB_SecPerClus db 1
    BPB_RsvdSecCnt dw 1
    BPB_NumFATs    db 2
    BPB_RootEntCnt dw 224
    BPB_TotSec16   dw 2880
    BPB_Media      db 0xF0
    BPB_FATSz16    dw 9
    BPB_SecPerTrk  dw 18
    BPB_NumHeads   dw 2
    BPB_HiddSec    dd 0
    BPB_TotSec32   dd 0
    BS_DrvNum      db 0
    BS_Reserved1   db 0
    BS_BootSig     db 0x29
    BS_VolID       dd 0
    BS_VolLab      db "YLOS-0     "
    BS_FileSysType db "FAT12   "

start:
    mov ax, cs
    mov ss, ax
	mov ds, ax
	mov es, ax
    mov sp, BaseOfStack

    mov ax, RootDirSec
    mov cx, RootDirSize
    mov bx, Buf
    call ReadSector

    mov si, Buf
    mov di, target
    mov dx, [BPB_RootEntCnt]  
    call FindEntry

    cmp dx, 0
    jz end

    ; mov si, target
    mov di, entry
    mov cx, EntryLength
    call MemCpy

    mov ax, FatTableSec
    mov cx, FatTableSize
    mov bx, Buf
    call ReadSector
    
    mov si, Buf
    mov ax, word [entry + 0x1a]

    mov cx, 1
    mov bx, BaseOfLoader
loading:
    add ax, 31
    call ReadSector
    sub ax, 31
    add bx, [BPB_BytsPerSec]

    call FatVec
    cmp ax, 0x7ff
    jna loading

    jmp BaseOfLoader

end:
    hlt
    jmp end

;----------------Print--------------------;
; es:bp --> str addr
; cx    --> str len
Print:
    push ax
    push bx

    mov ax, 0x1301
    mov bx, 0x0007
    int 0x10

    pop bx
    pop ax

    ret
;----------------Print--------------------;

;----------------ResetFloppy--------------;
; no input parameter
ResetFloppy:
    push ax
    push dx

    mov ah, 0x00
    mov dl, 0x00
    int 0x13

    pop dx
    pop ax

    ret
;----------------ResetFloppy--------------;

;----------------ReadSector---------------;
; ax    --> logical sector number
; cx    --> sector num
; es:bx --> target addr
ReadSector:
    push ax
    push bx
    push cx
    push dx

    call ResetFloppy

    push cx

    mov dl, [BPB_SecPerTrk]
    div dl

    mov cl, ah
    add cl, 1
    mov dh, al
    and dh, 1
    mov ch, al
    shr ch, 1
    mov dl, [BS_DrvNum]

    pop ax

    mov ah, 0x02

read:
    int 0x13
    jc read

    pop dx
    pop cx
    pop bx
    pop ax

    ret
;----------------ReadSector---------------;

;----------------MemCmp-------------------;
; si memory A
; di memory B
; cx len
; return:
;     cx == 0 ? equal : notequal 
MemCmp:
    push ax
    push si

cmpnext:
    mov al, byte [si]
    cmp al, byte [di]
    jne notequal

    inc si
    inc di
    dec cx
    cmp cx, 0
    jz equal

    jmp cmpnext

equal:
notequal:
    pop si
    pop ax
    ret
;----------------MemCmp-------------------;

;----------------FindEntry----------------;
; es:si --> root dir
; ds:di --> target file name
; dx    --> file num
; return:
;     si --> the target entry offset if exist
;     dx == 0 ? notexist : exist
FindEntry:
    mov cx, FielNameLength
    call MemCmp
    cmp cx, 0
    jz exist

    add si, EntryLength
    dec dx
    cmp dx, 0
    jnz FindEntry

notexist:
    mov bp, errorinfo
    mov cx, errorlen
    call Print
    ret

exist:
    ret
;----------------FindEntry----------------;

;----------------MemCpy-------------------;
; si --> source addr
; di --> destination addr
; cx --> length
MemCpy:
    push ax
    cmp si, di
    ja btoeinit

etob:
    mov al, byte [si]
    mov [di], al
    inc si
    inc di
    dec cx
    cmp cx, 0
    jz cpyfinish
    jmp etob

btoeinit:
    add si, cx
    sub si, 1
    add di, cx
    sub di, 1

btoe:
    mov al, byte [si]
    mov [di], al
    dec si
    dec di
    dec cx
    cmp cx, 0
    jz cpyfinish
    jmp btoe


cpyfinish:
    pop ax
    ret
;----------------MemCpy-------------------;

;----------------FatVec-------------------;
; ax --> index
; si --> fat table addr
; return:
;     ax --> fat[index]
FatVec:
    push cx
    push si

    mov cl, 2
    div cl
    cmp ah, 0
    jnz odd

even:
    mov cl, 3
    mul cl
    add si, ax
    mov ah, byte [si + 1]
    and ah, 0x0f
    mov al, byte [si]
    jmp return

odd:
    mov cl, 3
    mul cl
    add si, ax
    mov ah, byte [si + 2]
    mov al, byte [si + 1]
    shr ax, 4
    
return:
    pop si
    pop cx

    ret
;----------------FatVec-------------------;

msg:
    entry times EntryLength db 0x00
    errorinfo db "No Loader..."
    errorlen equ $ - errorinfo
    target db "LOADER  BIN"
    tarlen equ $ - target

Buf:
    times 510 - ($ - $$) db 0x00
    db 0x55, 0xaa

