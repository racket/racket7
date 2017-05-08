(library (expander)
  (export find-library-collection-links
          current-library-collection-links
          current-library-collection-paths
          find-library-collection-paths
          flush
          call-in-main-thread
          set-exec-file!
          version)
  (import (chezpart)
          (rename (core)
                  ;; These names are not public primitives, so "expander.scm"
                  ;; can define them:
                  [correlated? core:correlated?]
                  [correlated-source core:correlated-source]
                  [correlated-line core:correlated-line]
                  [correlated-column core:correlated-column]
                  [correlated-position core:correlated-position]
                  [correlated-span core:correlated-span]
                  [correlated-e core:correlated-e]
                  [correlated->datum core:correlated->datum]
                  [datum->correlated core:datum->correlated]
                  [correlated-property core:correlated-property]
                  [correlated-property-symbol-keys core:correlated-property-symbol-keys])
          (thread)
          (regexp)
          (io)
          (linklet))
  (include "expander-compat.scm")
  (include "compiled/expander.scm")
  (fill-environment!))
