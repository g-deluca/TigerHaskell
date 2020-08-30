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

mov $2, %eax

mov $4, %eax

push %eax

mov $5, %eax

push %eax

push %ebp

call g

add $12, %esp

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

sub $8, %esp

L6:

mov $12, %ecx

mov %ebp, %eax

add %ecx, %eax

mov (%eax), %eax

jmp L5

L5:

 
add $8, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


