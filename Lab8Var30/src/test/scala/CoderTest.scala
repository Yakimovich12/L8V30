import scala.collection.mutable

class CoderTest extends org.scalatest.FunSuite
{
  test("test0")
  {
    val str="teSt sTriNg"
    val result=main.scala.Coder.Encode(str)
    assert(result.equals("xaGx gXzbYk"))
  }
  test("test1")
  {
    val str="тесТоВаЯ строкА"
    val result=main.scala.Coder.Encode(str)
    assert(result.equals("кигКлЪэЩ гкдлтЭ"))
  }
  test("test2")
  {
    val str="xaGx gXzbYk"
    val result=main.scala.Coder.Decode(str)
    assert(result.equals("teSt sTriNg"))
  }
  test("test3")
  {
    val str="кигКлЪэЩ гкдлтЭ"
    val result=main.scala.Coder.Decode(str)
    assert(result.equals("тесТоВаЯ строкА"))
  }
  test("test4")
  {
    val elem=new main.scala.Coder.Element("name1","info","123")
    val list=mutable.LinkedList(elem)
    assert(main.scala.Coder.CheckPassword(list,"123")== true)
  }
  test("test5")
  {
    val elem=new main.scala.Coder.Element("name1","info","123")
    val list=mutable.LinkedList(elem)
    assert(main.scala.Coder.CheckPassword(list,"12")== false)
  }
  test("test6")
  {
    val elem=new main.scala.Coder.Element("name1","info","123")
    val list=mutable.LinkedList(elem)
    val element=main.scala.Coder.SearchInformation("name1",list)
    assert(list==element)
  }

  test("test7")
  {
    val elem=new main.scala.Coder.Element("name1","info","123")
    val list=mutable.LinkedList(elem)
    val element=main.scala.Coder.SearchInformation("info",list)
    assert(element==Nil)
  }
  test("test8")
  {
    val elem1=new main.scala.Coder.Element("name1","info","123")
    var list=mutable.LinkedList[main.scala.Coder.Element]()
    list=main.scala.Coder.AddNewElement(elem1,list)
    assert(list.elem.getInfo().equals(elem1.getInfo()))
    assert(list.elem.getName().equals(elem1.getName()))
    assert(list.elem.getPassword().equals(elem1.getPassword()))
  }
  test("test9")
  {
    val elem1=new main.scala.Coder.Element("name1","info1","123")
    val elem2=new main.scala.Coder.Element("name2","info2","1234")
    var list=mutable.LinkedList[main.scala.Coder.Element]()
    list=main.scala.Coder.AddNewElement(elem1,list)
    list=main.scala.Coder.AddNewElement(elem2,list)
    assert(list.next.elem.getInfo().equals(elem1.getInfo()))
    assert(list.next.elem.getName().equals(elem1.getName()))
    assert(list.next.elem.getPassword().equals(elem1.getPassword()))
    assert(list.elem.getInfo().equals(elem2.getInfo()))
    assert(list.elem.getName().equals(elem2.getName()))
    assert(list.elem.getPassword().equals(elem2.getPassword()))
  }

}
