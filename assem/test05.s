.data
L16:
.long 4
.string "chau"
L15:
.long 12
.string "como estas?
"
L6:
.long 16
.string "me tengo que ir
"
L5:
.long 6
.string "hola, "

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

L22:

mov $1, %eax

push %eax

push %ebp

call print_saludo

add $8, %esp

jmp L21

L21:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret

.globl print_saludo2

.type print_saludo2, @function

print_saludo2:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L24:

mov 12(%ebp), %ebx

mov $1, %eax

cmp %eax, %ebx

je L11

jmp L12

L12:

mov $0, %eax

mov %eax, %ebx

L13:

mov $0, %eax

cmp %eax, %ebx

jne L17

jmp L18

L18:

mov $L16, %eax

push %eax

call print

add $4, %esp

mov $0, %eax

L19:

jmp L23

L11:

mov $1, %eax

mov %eax, %ebx

jmp L13

L17:

mov $L15, %eax

push %eax

call print

add $4, %esp

mov $2, %eax

push %eax

push %ebp

call print_saludo

add $8, %esp

mov $0, %eax

jmp L19

L23:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret

.globl print_saludo

.type print_saludo, @function

print_saludo:

push %ebp

mov %esp, %ebp

push %edi

push %esi

push %esp

push %ebx

push %ebp

sub $0, %esp

L26:

mov 12(%ebp), %ebx

mov $1, %eax

cmp %eax, %ebx

je L1

jmp L2

L2:

mov $0, %eax

mov %eax, %ebx

L3:

mov $0, %eax

cmp %eax, %ebx

jne L7

jmp L8

L8:

mov $L6, %eax

push %eax

call print

add $4, %esp

mov $2, %eax

push %eax

push %ebp

call print_saludo2

add $8, %esp

mov $0, %eax

L9:

jmp L25

L1:

mov $1, %eax

mov %eax, %ebx

jmp L3

L7:

mov $L5, %eax

push %eax

call print

add $4, %esp

mov $1, %eax

push %eax

push %ebp

call print_saludo2

add $8, %esp

mov $0, %eax

jmp L9

L25:

 
add $0, %esp

pop %ebp

pop %ebx

pop %esp

pop %esi

pop %edi

pop %ebp

ret


