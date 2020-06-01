package main.scala
import scala.collection.mutable.LinkedList
import scala.io.StdIn.{readInt, readLine, _}
import scala.Boolean._
object Coder {
  class Element
  {
    private var name:String=""
    private var info:String=""
    private var password:String=""

    def this(name:String,info:String,password:String)=
    {
      this()
      this.name=name
      this.info=info
      this.password=password

    }
    def getName():String={this.name}
    def setName(name:String):Unit={this.name=name}
    def getPassword():String={this.password}
    def setPassword(password:String):Unit={this.password=password}
    def getInfo():String={this.info}
    def setInfo(info:String):Unit={this.info=info}
  }

  def main(arg:Array[String]):Unit=
  {
    var list=LinkedList[Element]()
    var code:Int=0
    do
    {
      println("Выбирите код операции:")
      println("1- Добавить информацию")
      println("2- Просмотреть информацию")
      println("3- Закодировать информацию")
      println("4- Раскодировать информацию")
      println("0- Выход")
      code=readInt()
      code match
      {
        case 1 =>
        {
          println("Введите название информации")
          val name=readLine()
          println("Введите информацию для шифрования")
          val info=readLine()
          println("Введите пароль для доступа к информации")
          val password=readLine()
          val elem=new Element(name,info,password)
          list=AddNewElement(elem,list)
        }
        case 2 =>
        {
          println("Введите название информации которую нужно найти и прочитать")
          val name=readLine()
          val current=SearchInformation(name,list)
          if(current!=Nil)
            PrintFunction(current)
          else
            println("Записи с данным названием не существует")
        }
        case 3 =>
        {
          println("Введите название информации которую нужно найти и закодировать")
          val name=readLine()
          val current=SearchInformation(name,list)
          if(current!=Nil)
          {
            println("Введите пароль к данной записи")
            val password=readLine()
            if(CheckPassword(current,password))
            {
              current.elem.setInfo(Encode(current.elem.getInfo()))
              println("Запись закодирована")
            }
            else
              println("Пароль неверный")
          }
          else
            println("Данных с таким именем")
        }
        case 4 =>
        {
          println("Введите название информации которую нужно найти и раскодировать")
          val name=readLine()
          val current=SearchInformation(name,list)
          if(current!=Nil)
          {
            println("Введите пароль к данной записи")
            val password=readLine()
            if(CheckPassword(current,password))
            {
              current.elem.setInfo(Decode(current.elem.getInfo()))
              println("Запись раскодирована")
            }
            else
              println("Пароль неверный")
          }
          else
            println("Данных с таким именем")
        }

        case _ =>
        {}
      }
    }while(code!=0)


  }
  def PrintFunction(current:LinkedList[Element]):Unit=
  {
    if(current!=Nil)
    {
      println("Название информации:")
      println(current.elem.getName())
      println("Содержимое:")
      println(current.elem.getInfo())
    }
    else
      println("Нет информации для вывода")
  }

  def CheckPassword(cur:LinkedList[Element],password:String):Boolean=
  {
    if(password.equals(cur.elem.getPassword()))
      true
    else
      false
  }

  def SearchInformation(name:String,list:LinkedList[Element]):LinkedList[Element]=
  {
    var cur=list
    var res=LinkedList[Element]()
    while(cur!=Nil)
    {
      if(name.equals(cur.elem.getName()))
        res=cur

      cur=cur.next
    }
    res
  }


  def AddNewElement(elem:Element,list:LinkedList[Element]):LinkedList[Element]=
  {
    val cur=LinkedList(elem)
    if(list!=Nil)
      cur.next=list
    cur
  }

  def Encode(str:String):String=
  {
    var result=""
    val key:String =    "qwertyuioplkjhgfdsazxcvbnmюбьтимсчяфывапролджэъхзщшгнекуцй"
    val string:String = "zxcvbnmlkjhgfdsaqwertyuiopйцукенгшщзхъэждлорпавыфячсмитьбю"
    for(i<- 0 to str.length()-1)
    {
      val symbol=str.charAt(i)
      val first=string.indexOf(symbol)
      if(first!= -1)
        result+=key.charAt(first)
      else
      {
        val second = string.toUpperCase.indexOf(symbol)
        if(second != -1)
          result+=key.toUpperCase.charAt(second)
        else
          result += symbol
      }
    }
    result
  }
  def Decode(str: String):String=
  {
    var result=""
    val key:String =    "zxcvbnmlkjhgfdsaqwertyuiopйцукенгшщзхъэждлорпавыфячсмитьбю"
    val string:String = "qwertyuioplkjhgfdsazxcvbnmюбьтимсчяфывапролджэъхзщшгнекуцй"
    for(i<- 0 to str.length()-1)
    {
      val symbol=str.charAt(i)
      val first=string.indexOf(symbol)
      if(first!= -1)
        result+=key.charAt(first)
      else
      {
        val second = string.toUpperCase.indexOf(symbol)
        if(second != -1)
          result+=key.toUpperCase.charAt(second)
        else
          result += symbol
      }
    }
    result
  }
}