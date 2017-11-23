package fpinScala.FindFirstString

object FindFirstString{
    def findFirst(ss:Array[String],key:String):Int={
        @annotation.tailrec
        def find(n:Int):Int={
            if (n>=ss.length) -1
            else if (ss(n)==key) n
            else find(n+1)
        }
        find(0)
    }

    def main(args:Array[String]):Unit={
        val str1=Array("abc","qwe")
        val key1="abc"
        println(findFirst(str1,key1))
    }
}