.data
L5:
.long 4
.string "str2"
L4:
.long 1
.string " "
L3:
.long 3
.string "str"
L2:
.long 12
.string "do_nothing2
"
L1:
.long 12
.string "do_nothing1
"

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

L7:

mov $L5, %eax

push %eax

mov $0, %eax

push %eax

push %ebp

call do_nothing1

add $12, %esp

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

L9:

mov $L2, %eax

push %eax

call print

add $4, %esp

mov $L3, %eax

push %eax

mov 12(%ebp), %eax

push %eax

push %ebp

call do_nothing1

add $12, %esp

mov $L4, %eax

jmp L8

L8:

 
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

L11:

mov $L1, %eax

push %eax

call print

add $4, %esp

mov 12(%ebp), %ebx

mov $1, %eax

add %ebx, %eax

push %eax

push %ebp

call do_nothing2

add $8, %esp

mov $0, %eax

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


