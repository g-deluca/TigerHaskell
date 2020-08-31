.data
L10:
.long 10
.string "No es cero"
L9:
.long 7
.string "Es cero"

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

L17:

mov $3, %eax

push %eax

mov $10, %eax

push %eax

call _allocArray

add $8, %esp

mov $-20, %edx

mov %ebp, %ecx

add %edx, %ecx

mov %eax, (%ecx)

mov $42, %eax

mov $-24, %edx

mov %ebp, %ecx

add %edx, %ecx

mov %eax, (%ecx)

mov $-20, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %eax

mov $-20, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %edx

mov $0, %eax

mov $4, %ecx

imul %ecx, %eax

mov %edx, %ecx

add %eax, %ecx

mov (%ecx), %ecx

mov $0, %eax

cmp %eax, %ecx

je L5

jmp L6

L6:

mov $0, %eax

L7:

mov $0, %ecx

cmp %ecx, %eax

jne L11

jmp L12

L12:

mov $L10, %eax

push %eax

call print

add $4, %esp

mov $0, %eax

L13:

mov $42, %eax

push %eax

push %ebp

call g

add $8, %esp

jmp L16

L5:

mov $1, %eax

jmp L7

L11:

mov $L9, %eax

push %eax

call print

add $4, %esp

mov $0, %eax

jmp L13

L16:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret

.globl g

.type g, @function

g:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L19:

mov $8, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %eax

mov $-24, %ecx

add %ecx, %eax

mov (%eax), %eax

push %eax

mov $8, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %eax

mov $-20, %ecx

add %ecx, %eax

mov (%eax), %edx

mov $0, %eax

mov $4, %ecx

imul %ecx, %eax

mov %edx, %ecx

add %eax, %ecx

mov (%ecx), %eax

mov -84(%ebp), %ecx

add %eax, %ecx

mov %ecx, %eax

jmp L18

L18:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


