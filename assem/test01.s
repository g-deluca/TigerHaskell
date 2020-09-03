.data
L9:
.long 10
.string "No es cero"
L8:
.long 7
.string "Es cero"

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

L16:

mov $3, %eax

push %eax

mov $10, %eax

push %eax

call _allocArray

add $8, %esp

mov $-20, %ecx

add %ebp, %ecx

mov %eax, (%ecx)

mov -20(%ebp), %eax

mov -20(%ebp), %edx

mov $0, %ecx

mov $4, %eax

imul %ecx, %eax

add %edx, %eax

mov (%eax), %ecx

mov $0, %eax

cmp %eax, %ecx

je L4

jmp L5

L5:

mov $0, %eax

L6:

mov $0, %ecx

cmp %ecx, %eax

jne L10

jmp L11

L11:

mov $L9, %eax

push %eax

call print

add $4, %esp

mov $0, %eax

L12:

mov $42, %eax

push %eax

push %ebp

call g

add $8, %esp

jmp L15

L4:

mov $1, %eax

jmp L6

L10:

mov $L8, %eax

push %eax

call print

add $4, %esp

mov $0, %eax

jmp L12

L15:

 
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

L18:

mov 8(%ebp), %eax

mov -20(%eax), %edx

mov $0, %ecx

mov $4, %eax

imul %ecx, %eax

add %edx, %eax

mov (%eax), %ecx

mov 12(%ebp), %eax

add %ecx, %eax

jmp L17

L17:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


