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

L4:

mov $7, %eax

mov $10, %eax

mov %eax, %edi

mov $-20, %eax

add %ebp, %eax

mov %edi, (%eax)

push %ebx

push %ecx

push %ebp

call g

add $4, %esp

pop %ebx

pop %ecx

jmp L3

L3:

 
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

L6:

mov 8(%ebp), %eax

mov -20(%eax), %eax

jmp L5

L5:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


