.data
L9:
.long 8
.string "arr[1]: "
L8:
.long 1
.string "
"
L7:
.long 8
.string "arr[0]: "
L5:
.long 7
.string "newells"
L2:
.long 4
.string "hola"

.text
.globl _tigermain

.type _tigermain, @function

_tigermain:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L11:

mov $L2, %eax

push %eax

mov $10, %eax

push %eax

call _allocArray

add $8, %esp

mov %eax, %ebx

mov $L5, %eax

mov $0, %edi

mov $4, %ecx

imul %edi, %ecx

add %ebx, %ecx

mov %eax, (%ecx)

mov $L7, %eax

push %eax

call print

add $4, %esp

mov $0, %ecx

mov $4, %eax

imul %ecx, %eax

add %ebx, %eax

mov (%eax), %eax

push %eax

call print

add $4, %esp

mov $L8, %eax

push %eax

call print

add $4, %esp

mov $L9, %eax

push %eax

call print

add $4, %esp

mov $1, %ecx

mov $4, %eax

imul %ecx, %eax

add %ebx, %eax

mov (%eax), %eax

push %eax

call print

add $4, %esp

jmp L10

L10:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


