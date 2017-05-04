This package implements the port, path, encoding, printing,
and formatting layer.

Core error support must be provided as a more primitive layer,
including the exnception structures and error functions that do not
involve formatting, such as `raise-argument-error`. The more primitive
layer should provide a `error-value->string-handler` paramemeter, but
this layer sets that parameter (so the primitive error function slike
`raise-argument-error` won't work right until this layer is loaded).
