org 0x9000

start:
    mov dx, 0
    mov bp, msg
    mov cx, 6
    mov ax, 0x1301
    mov bx, 0x0007
    int 0x10

end:
    hlt
    jmp end

msg:
    db "hello!"