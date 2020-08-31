.data

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

L5:

mov $7, %eax

mov $10, %eax

mov $-20, %edx

mov %ebp, %ecx

add %edx, %ecx

mov %eax, (%ecx)

push %ebp

call g

add $4, %esp

jmp L4

L4:

 
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

L7:

mov $8, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %ecx

mov $-20, %eax

add %eax, %ecx

mov (%ecx), %eax

jmp L6

L6:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


