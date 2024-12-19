/* hack to define new assembly instructions */

.macro INC r
        .byte (1 << 4) | \r
.endm

.macro HALT
        .byte 0
.endm

/* hack to define register names */
#define R0 0
#define R1 1
#define R2 2
#define R3 3
