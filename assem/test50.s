.data
L2:
.long 19
.string "about to break loop"

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

L6:

L4:

push %ebx

push %ecx

mov $L2, %eax

push %eax

call print

add $4, %esp

pop %ebx

pop %ecx

L1:

jmp L7

L3:

jmp L5

L7:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


