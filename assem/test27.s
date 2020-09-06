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

L3:

mov $2, %eax

push %ebx

push %ecx

mov $4, %eax

push %eax

mov $5, %eax

push %eax

push %ebp

call g

add $12, %esp

pop %ebx

pop %ecx

jmp L2

L2:

 
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

L5:

mov 12(%ebp), %eax

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


