.data
L5:
.long 8
.string "Somebody"
L2:
.long 6
.string "Nobody"

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

L8:

mov $L2, %eax

push %eax

mov $1000, %eax

push %eax

mov $2, %eax

push %eax

call _allocRecord

add $12, %esp

mov $L5, %ecx

mov $4, %edx

add %eax, %edx

mov %ecx, (%edx)

mov 4(%eax), %eax

push %eax

call print

add $4, %esp

jmp L7

L7:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


