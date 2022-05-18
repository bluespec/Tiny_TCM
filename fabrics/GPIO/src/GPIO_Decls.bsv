// Copyright (c) 2022- Bluespec, Inc. All Rights Reserved.
// This package captures definitions used by the GPIO adapter

package GPIO_Decls;

import Vector        :: *;

// Number of GPIOs in each direction
`ifdef GPIO_8
typedef 8 NUM_GPIO;
`elsif GPIO_16
typedef 16 NUM_GPIO;
`elsif GPIO_32
typedef 32 NUM_GPIO;
`endif

typedef Bit    #(NUM_GPIO)             GPIO_Word;
typedef Vector #(NUM_GPIO, Bit #(1))   GPIO_Word_V;
typedef TLog   #(NUM_GPIO)             Bits_per_GPIO_Index;
typedef Bit    #(Bits_per_GPIO_Index)  GPIO_Index;

// LSBs to address a GPIO
typedef TAdd# (Bits_per_GPIO_Index, TLog #(32)) GPIO_Addr_LSB;
Integer gpio_addr_lsb = valueOf (GPIO_Addr_LSB);

endpackage
