# Scala implementation of Day 8

## Algorithms
### part 1
No special algorithms. Just follow the graph until reaching the target node.
### part 2
I'm using a Priority Queue to quickly insert a node into it and retrieve the least advanced node. When the least advanced node is updated to its next target node, I check if all nodes have the same distance. If yes that is my answer, otherwise I repeat this process.

## Performances
### part 1
- Compiling + running using `scala part1.scala input.txt` takes around 2.151s.
- Running only with `./Part1JVM input.txt` (packaged with scala-cli) takes around 0.183s
- Running the native version using scala native with `./Part1Native input.txt` takes around 0.005s

### part 2
- Compiling + running using `scala part2.scala input.txt` takes around 15m57.537s.
- Running only with `./Part2JVM input.txt` (packaged with scala-cli) takes around 10m14.267s
- Running the native version using scala native with `./Part2Native input.txt` crashes without GC, takes around 19m2.958s with default GC

The native version is slower...

