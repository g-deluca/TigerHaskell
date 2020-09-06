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

push %ebx

push %ecx

mov $0, %eax

push %eax

mov $10, %eax

push %eax

call _allocArray

add $8, %esp

pop %ebx

pop %ecx

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


