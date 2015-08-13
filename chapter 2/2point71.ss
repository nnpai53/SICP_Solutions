;; huffman tree for symbols having relative frequency 1,2,4,...2^n-1.
;; sketching the sample tree for n=5. For n=10 as well, the structure of the
;; tree will be the same, only it will be a bit larger.

{a,b,c,d,e}-31
/             \
{b,c,d,e}-15  {a}-16
/         \
{c,d,e}-7  {b}-8
/        \
{d,e}-3  {c}-4
/     \
{d}-1  {e}-2

;; The number of bits needed to encode the most frequent symbol
;; in this case as we can see from above is always 1.
;; In order to encode the least frequent symbol the number of bits needed
;; is n-1.