//> using platform "scala-native"
//> using nativeMode "release-full"
//> using nativeGc "none"
import scala.util.control.TailCalls.TailRec
import scala.annotation.tailrec

object Part2Tries {
    import scala.collection.mutable.HashMap
    import Part2Tries.Trie.{withWord, create, createBackward}

    val templateTrie = Trie
            .withWord("one", 1)
            .withWord("two", 2)
            .withWord("three", 3)
            .withWord("four", 4)
            .withWord("five", 5)
            .withWord("six", 6)
            .withWord("seven", 7)
            .withWord("eight", 8)
            .withWord("nine", 9)
            .withWord("1", 1)
            .withWord("2", 2)
            .withWord("3", 3)
            .withWord("4", 4)
            .withWord("5", 5)
            .withWord("6", 6)
            .withWord("7", 7)
            .withWord("8", 8)
            .withWord("9", 9)
   

    def main(args: Array[String]): Unit =
        
        val lines = scala.io.Source.fromFile(args(0)).getLines().filter(!_.isBlank())
        val forwardTrie = templateTrie.create()
        val backwardTrie = templateTrie.createBackward()
        println(lines.map(line => Trie.findFirstWordInSeq(line, forwardTrie) * 10 + Trie.findFirstWordInSeq(line.reverse, backwardTrie)).sum)
        

    object Trie:
        type TrieBuilder = List[(String, Int)]
        extension (tb: TrieBuilder)
            def withWord(s: String, v: Int): TrieBuilder = (s, v)::tb
            def createBackward(): TrieNode = create(reverse=true)
            def create(reverse: Boolean=false): TrieNode = 
                case class MutableTrieNode(key: Char, value: Option[Int], kids: HashMap[Char, MutableTrieNode]):
                    def toTrieNode: TrieNode = TrieNode(key, value, kids.map{case k -> v => (k, v.toTrieNode)}.toMap)
                val root = MutableTrieNode('*', None, HashMap.empty[Char, MutableTrieNode])
                def insert(currentNode: MutableTrieNode, word: Seq[Char], value: Int): Unit = word match
                    case Seq() => ()
                    case lastLetter +: Seq() => currentNode.kids.put(lastLetter, MutableTrieNode(lastLetter, Some(value), HashMap.empty)) // Special case for this exercise: Prune the trie if end value is defined earlier
                    case l +: remainingWord => 
                        insert(currentNode.kids.getOrElseUpdate(l, MutableTrieNode(l, None, HashMap.empty)), remainingWord, value)
                        
                
                tb.foreach{case (word, value) => insert(root, if reverse then word.reverse.toSeq else word.toSeq, value)}
                root.toTrieNode



        def withWord(s: String, v: Int): TrieBuilder = List((s, v))
        def findFirstWordInSeq(s: String, root: TrieNode): Int = 
            def helper(s: Seq[Char]): Option[Int] = s match
                case Seq() => Some(0)
                case str => root.findValue(str).orElse(helper(str.tail))
            helper(s.toSeq).get
            
        

    case class TrieNode(key: Char, value: Option[Int], kids: Map[Char, TrieNode]):
        override def toString(): String = toString(1)
        def toString(indent: Int): String = s"[$key]${value.map(v => s"($v)").getOrElse("")}: \n${kids.map(kid => (" | " * indent) + kid._2.toString(indent + 1)).mkString}"
        def findValue(s: Seq[Char]): Option[Int] = s match
            case Seq() => None
            case l +: tail =>
                kids.get(l)
                    .flatMap{node => node.value
                                        .orElse(node.findValue(tail))}
        
}
