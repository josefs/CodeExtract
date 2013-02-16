# Code Extract

This program enables fine grained control over extracting code in a
pandoc markdown into files. Code blocks can be
annotated with the attribute `file` to specify
what file the code should be extracted to. If the
same file attribute is given for several code
blocks then these code blocks are concatenated
into the specified file.
