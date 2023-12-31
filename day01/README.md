#Scala implementation of Day 1

## Algorithms
### part 1
No special algorithms.
### part 2 (simple)
The trick is to replace for example "one" by "one1one" and then filter all letters.
By doing so, we don't erase information when looking for eights for in the string "oneight" for example
### part 2 (tries)
Custom implementation of a Trie data structure. Then, we loop over each character of a line until having our Trie find a matching word.

## Performances
### part 1
- Compiling + running using `scala part1.scala input.txt` takes around 1.910s.
- Running only with `./Part1JVM input.txt` (packaged with scala-cli) takes around 0.183s
- Running the native version using scala native with `./Part1Native input.txt` takes around 0.002s

The native version is 90x faster

### part 2 (Simple)
- Compiling + running using `scala part2Simple.scala input.txt` takes around 1.938s.
- Running only with `./Part2JVM input.txt` (packaged with scala-cli) takes around 0.210s
- Running the native version using scala native with `./Part2Native input.txt` takes around 0.002s

The native version is 105x faster

### part 2 (Tries)
- Compiling + running using `scala part2Tries.scala input.txt` takes around 2.59s.
- Running only with `./Part2JVM input.txt` (packaged with scala-cli) takes around 0.244s
- Running the native version using scala native with `./Part2Native input.txt` takes around 0.005s

The native version is 11x faster.
Surprisingly, the tries version is slower than the simple one.
Theoritically, I guess it can perform better if the size of the dictionary (the numbers "one" etc) is getting bigger.