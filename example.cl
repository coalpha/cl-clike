(import "clike.cl")

(clike
   let i = 0
   while <(i 10) {
      if =(0 mod(i 2)) {
         princ("nope")
         i = +(i 1)
         continue()
      }

      print(i)
      terpri <- nil
      i = +(i 1)
   }
   exit <- nil
)
