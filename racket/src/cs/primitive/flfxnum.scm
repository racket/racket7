
(define flfxnum-table
  (make-primitive-table
   
   fx->fl fl->fx
   fxabs
   fx+ fx- fx*
   fxquotient fxremainder fxmodulo 
   fxand fxior fxxor
   fxnot fxrshift fxlshift
   fx>= fx> fx= fx< fx<=
   fxmin fxmax
   fxvector? fxvector make-fxvector 
   shared-fxvector make-shared-fxvector
   fxvector-length fxvector-ref fxvector-set!
   fxvector-copy

   fl+ fl- fl* fl/
   flabs flsqrt flexp fllog
   flsin flcos fltan flasin flacos flatan
   flfloor flceiling flround fltruncate flexpt
   fl= fl< fl<= fl> fl>= flmin flmax
   ->fl fl->exact-integer
   flvector? flvector make-flvector 
   shared-flvector make-shared-flvector
   flvector-length flvector-ref flvector-set!
   flvector-copy
   flreal-part flimag-part make-flrectangular))
