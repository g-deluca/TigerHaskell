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

L10:

push %ebx

push %ecx

mov $5, %eax

push %eax

push %ebp

call fact

add $8, %esp

pop %ebx

pop %ecx

jmp L9

L9:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret

.globl fact

.type fact, @function

fact:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L14:

mov 12(%ebp), %ebx

mov $0, %eax

cmp %eax, %ebx

je L1

jmp L2

L2:

mov $0, %eax

L3:

mov $0, %ebx

cmp %ebx, %eax

jne L5

jmp L6

L6:

mov 12(%ebp), %eax

mov %eax, %edi

push %ebx

push %ecx

mov 12(%ebp), %ebx

mov $1, %eax

sub %eax, %ebx

push %ebx

push %ebp

call fact

add $8, %esp

pop %ebx

pop %ecx

imul %edi, %eax

L7:

jmp L13

L1:

mov $1, %eax

jmp L3

L5:

mov $1, %eax

jmp L7

L13:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


