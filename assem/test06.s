.data
L2:
.long 4
.string "str2"
L1:
.long 3
.string "str"

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

push %ebx

push %ecx

mov $L2, %eax

push %eax

mov $0, %eax

push %eax

push %ebp

call do_nothing1

add $12, %esp

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

.globl do_nothing2

.type do_nothing2, @function

do_nothing2:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L6:

push %ebx

push %ecx

mov $L1, %eax

push %eax

mov 12(%ebp), %eax

push %eax

push %ebp

call do_nothing1

add $12, %esp

pop %ebx

pop %ecx

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

.globl do_nothing1

.type do_nothing1, @function

do_nothing1:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L8:

push %ebx

push %ecx

mov 12(%ebp), %ebx

mov $1, %eax

add %ebx, %eax

push %eax

push %ebp

call do_nothing2

add $8, %esp

pop %ebx

pop %ecx

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


