module Main where

import qualified Lib as Lib
import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import Test.HUnit
import System.IO.Unsafe

main :: IO ()
main = do
  Test.defaultMain $ Test.hUnitTestToTests $ TestList
    [
      "0" ~=? pe "0",
      "42" ~=? pe "42",
      "3" ~=? pe "1+2",
      "7" ~=? pe "1+2*3",
      "9" ~=? pe "(1+2)*3",
      "2" ~=? pe "3-1",
      "1" ~=? pe "a=1;a",
      "3" ~=? pe "a=1;b=2;a+b",
      "120" ~=? pe "fun fact : Int -> Int = {1->1; n->n*fact(n-1)}; fact 5",
      "11" ~=? pe "fun inc : Int -> Int = n -> n + 1; 10.inc",
      "3" ~=? pe "fun add : Int -> Int -> Int = x y -> x + y; add 1 2",
      "3" ~=? pe "fun add : Int -> Int -> Int = x y -> x + y; 1.add 2",
      "6" ~=? pe "fun add : Int -> Int -> Int = x y -> x + y; 1.add 2.add 3",
      "hello" ~=? pe "'hello'",
      "hello world" ~=? pe "'hello'+' world'",
      "hellohellohello" ~=? pe "'hello'*3",
      "3" ~=? pe "fun add : Int -> Int -> Int = x y -> x + y; fun add : String -> String -> String = x y -> x + y; 1+2",
      "ab" ~=? pe "fun add : Int -> Int -> Int = x y -> x + y; fun add : String -> String -> String = x y -> x + y; 'a'+'b'",
      "40" ~=? pe "fun double : Int -> Int = x -> x + x; fun twice : (Int->Int)->Int->Int = f x -> f (f x); twice (double:Int->Int) 10",
      "a a a a" ~=? pe "fun double : Int -> Int = x -> x + x; fun double : String -> String = x ->x + ' ' + x; fun twice : (String->String)->String->String = f x -> f (f x); twice (double:String->String) 'a'",
      "10" ~=? pe "fun id : a -> a = x -> x; id 10;",
      "hoge" ~=? pe "fun id : a -> a = x -> x; id 'hoge';",
      "40" ~=? pe "fun double : Int -> Int = x -> x + x; fun twice : (a->a)->a->a = f x -> f (f x); twice (double:Int->Int) 10",
      "40" ~=? pe "fun double : Int -> Int = x -> x + x; fun twice : (Int->Int)->Int->Int = f x -> f (f x); twice double 10",
      "10" ~=? pe "fun id : a -> a = x -> x; (id id) 10",
      "aaaa" ~=? pe "fun double : a -> a = x -> x + x; fun twice : (a->a)->a->a = f x -> f (f x); twice double 'a'",
      "40" ~=? pe "fun comp : (b->c) -> (a->b) -> a -> c = f g x -> f (g x); fun double : a->a = x -> x + x; comp double double 10",
      "20" ~=? pe "(x->x+x:Int->Int) 10",
      "aa" ~=? pe "(x->x+x:String->String) 'a'",
      "40" ~=? pe "(x->x+x:a->a) 20",
      "hogehoge" ~=? pe "(x->x+x:a->a) 'hoge'",
      "40" ~=? pe "fun twice : (a->a)->a = f x -> f (f x); twice (x->x+x:a->a) 10",
      "bbbb" ~=? pe "fun twice : (a->a)->a = f x -> f (f x); twice (x->x+x:a->a) 'b'",
      "20" ~=? pe "fun apply : (a->a)->(a->a) = f -> (x -> f x : a->a); fun double:a->a = x->x+x; (apply double) 10",
      "40" ~=? pe "fun twice : (a->a)->(a->a) = f -> (x -> f (f x) : a->a); fun double:a->a = x->x+x; (twice double) 10",
      "1024" ~=? pe "fun ** : Int -> Int -> Int = { a 0 -> 1; a b -> (a ** (b-1)) * a }; 2 ** 10",
      "3" ~=? pe "fun ** : (b->c) -> (a->b) -> (a->c) = f g -> (x -> x.g.f : a -> c); fun inc : Int -> Int = x -> x + 1; fun double : Int -> Int = x -> x * 2; (inc ** double) 1",
      "5" ~=? pe "fun length : [a] -> Int = { [] -> 0; es -> 1 + length (tail es) }; length [4,5,6,7,8]",
      "[2,4,6]" ~=? pe "fun map : [a] -> (a->a) -> [a] = { [] f -> []; es f -> [f (head es)] + (map (tail es) f)}; map [1,2,3] (x -> x + x : Int -> Int)",
      "[4,3,2,1]" ~=? pe "fun reverse : [a] -> [a] = { [] -> []; es -> (reverse (tail es)) + [head es] }; reverse [1,2,3,4]",
      "True" ~=? pe "True && True",
      "False" ~=? pe "True && False",
      "True" ~=? pe "True || True",
      "True" ~=? pe "True || False",
      "True" ~=? pe "True + True",
      "False" ~=? pe "True * False",
      "True" ~=? pe "1 < 2",
      "False" ~=? pe "1 == 2",
      "True" ~=? pe "1 == 1",
      "True" ~=? pe "[1,2] == [1,2]",
      "False" ~=? pe "1 > 2",
      "False" ~=? pe "(1 > 2) && (3 == 3)",
      "2" ~=? pe "if 1==1 then 2 else 3",
      "3" ~=? pe "if 1>1 then 2 else 3",
      "[1,2,3,4,5]" ~=? pe "fun select : [a] -> (a -> Bool) -> [a] = { [] f -> []; es f -> if (es.head.f) then ([es.head] + select (es.tail) f) else (select (es.tail) f) }; fun qsort : [a] -> [a] = { [] -> []; es  -> es.tail.select (x -> x < es.head : a -> Bool).qsort + es.select (x -> x == es.head : a -> Bool) + es.tail.select (x -> x > es.head : a -> Bool).qsort }; qsort [3,5,1,4,2]",
      "4.6" ~=? pe "1.2+3.4",
      "6.8" ~=? pe "2*3.4",
      "-3" ~=? pe "-3",
      "-48" ~=? pe "-4*12",
      "2.2" ~=? pe "fun add : Double -> Double = { x y -> x + y }; add 1.1 1.1",
      "[4,3,2,1]" ~=? pe "fun reverse : [a] -> [a] = { [] -> []; [h,t] -> (reverse t) + [h] }; reverse [1,2,3,4]",
      "3" ~=? pe "fun outer : a -> a = x -> { fun inner : a -> a -> a = x y -> x + y; inner 1 x }; outer 2",
      "1" ~=? pe "type Complex = { r:Int, i:Int }; a = Complex 1 2; a.r",
      "2" ~=? pe "type Complex = { r:Int, i:Int }; a = Complex 1 2; a.i",
      "4" ~=? pe "type Complex = { r:Int, i:Int }; fun add : Complex -> Complex = x y -> Complex (x.r+y.r) (x.i+y.i); a = Complex 1 2; b = Complex 3 4; (add a b).r",
      "4" ~=? pe "type Complex = { r:Int, i:Int }; fun + : Complex -> Complex = x y -> Complex (x.r+y.r) (x.i+y.i); a = Complex 1 2; b = Complex 3 4; (a + b).r",
      "8" ~=? pe "type Complex = { r:Int, i:Int }; fun * : Complex -> Complex = x y -> Complex (x.r*y.r) (x.i*y.i); a = Complex 1 2; b = Complex 3 4; (a * b).i",
      "3" ~=? pe "type Complex = { r:Int, i:Int }; fun + : Complex -> Int -> Complex = x y -> Complex (x.r+y) (x.i); a = Complex 1 2; (a + 2).r",
      "3" ~=? pe "type Complex = { r:Int, i:Int }; fun + : Int -> Complex -> Complex = y x -> Complex (x.r+y) (x.i); a = Complex 1 2; (2 + a).r",
      "2" ~=? pe "type Complex = { r:Int, i:Int }; fun - : Complex -> Complex = x y -> Complex (x.r-y.r) (x.i-y.i); a = Complex 1 2; b = Complex 3 4; (b - a).r",
      "3" ~=? pe "fun * : (b->c) -> (a->b) -> (a->c) = f g -> (x -> x.g.f : a -> c); fun inc : Int -> Int = x -> x + 1; fun double : Int -> Int = x -> x * 2; (inc * double) 1"

    ]

pe :: String -> String
pe program = unsafePerformIO (Lib.parseEval program)
