;; A crisp explanation found here (http://community.schemewiki.org/?sicp-ex-1.25) is written below

;; The modified version of expmod computes huge intermediate results.
;; Scheme is able to handle arbitrary-precision arithmetic, but arithmetic with arbitrarily long numbers is 
;; computationally expensive. This means that we get the same (correct) results, but it takes considerably longer.

;; This one provides proof by implementing the two versions practically
;; https://wizardbook.wordpress.com/2010/11/29/exercise-1-25/