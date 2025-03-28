# Supported platforms

ARMv8 in 64-bit mode (AArch64).

Debian architecture name: `arm64`.

# Reference documents

* Instruction set architecture:
  _ARM Architecture Reference Manual, ARMv8_, restricted to the AArch64 subset.
* Application binary interface:
  _Procedure Call Standard for the ARM 64-bit Architecture (AArch64)_
  _Apple ARM64 Function Calling Conventions_


# Regex Substitution to Eliminate `.mlp`

```
Zero Entries Regex
`([^\{]*)` -> emitp_format out "$1"


One Entry Regex
`([^\{]*)\{([^\}]*)\}([^\{]*)` -> emitp_format out "$1%a$3" $2


Two Entries Regex
`([^\{]*)\{([^\}]*)\}([^\{]*)\{([^\}]*)\}([^\{]*)` -> emitp_format out "$1%a$3%a$5" $2 $4


Three Entries Regex
`([^\{]*)\{([^\}]*)\}([^\{]*)\{([^\}]*)\}([^\{]*)\{([^\}]*)\}([^\{]*)` -> emitp_format out "$1%a$3%a$5%a$7" $2 $4 $6


Four Entries Regex
`([^\{]*)\{([^\}]*)\}([^\{]*)\{([^\}]*)\}([^\{]*)\{([^\}]*)\}([^\{]*)\{([^\}]*)\}([^\{]*)` -> emitp_format out "$1%a$3%a$5%a$7%a$9" $2 $4 $6 $8
```