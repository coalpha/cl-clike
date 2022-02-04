(load "clike.cl")

(clike
   let i = 0

   function exclaim(str) {
      function strcat(a b) {
         return(concatenate('string a b))
      }
      return(strcat(str "!"))
   }

   while <(i 10) {
      if =(0 mod(i 2)) {
         i = +(i 1)
         continue()
      }

      princ(exclaim(write-to-string(i)))
      terpri <- nil
      i = +(i 1)
   }
   exit <- nil
)
