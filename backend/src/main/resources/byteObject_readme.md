# byte communication implement.

## MiddleBuffer
1. `trait MiddleBuffer`是用于进行二进制转换的中间数据数据结构; 
2. `MiddleBuffer`提供了方便的`putXXX()`方法写入方法, 使我们可以将需要传输的对象, 保存到`MiddleBuffer`中; 
3. 它的具体实现提供了`result()`方法, 转换为方便传输的二进制形式, 用于传输; 
   * 在jvm平台上是`MiddleBufferInJvm`, 转换为`Array[Byte]`数组; 
   * 在js平台上是`MiddleBufferInJs`, 转换为`ArrayBuffer`数组; 
4. 同时, 完成传输后, 你也可以通过二进制数据方便的构造`MiddleBuffer`的具体实现; 
5. 最后`MiddleBuffer`提供了`getXXX()`方法来获取数据; 
6. 以上内容，在实际使用时，很多都不需要关注;

## byteObject
1. `byteObject`包提供了三个东西：`ByteEncoder`,`ByteDecoder`和`ByteObject`
2. `ByteEncoder`能够分析对象结构**自动的**将对象序列化到`MiddleBuffer`中;
3. 类似的, `ByteDecoder`能够自动的将`MiddlerBuffer`中的二进制数据解析为指定对象;


## Example
说了这么多, 其实使用起来非常非常简单, 代码在本工程的netSnake_back分支下； 
例子，假设消息定义如下：
 ```
   sealed trait Msg
   case class TextMsg(id: Int, data: String, value: Float) extends Msg
   case class MultiTextMsg(id: Int, b: Option[Boolean], ls: List[TextMsg]) extends Msg

 ```
 目前已经支持全部scala基础类型；
 
1. 将`trait MiddleBuffer`拷贝到shared下;
2. 将`MiddleBufferInJvm`拷贝到`backend`下;
3. 将`MiddleBufferInJs`拷贝到`frontend`下;
4. 将`byteObject`包以及包内文件分别拷贝到`frontend`和`backend`中；
以上操作可以参考`hiStream`工程`netSnake_back`分支下的结构来；


然后在需要使用编解码的文件中，引入编解码工具；
 ```
   import com.neo.sk.utils.byteObject.ByteObject._
 ```

### In Jvm
* encode  
  ```
  val sendBuffer = new MiddleBufferInJvm(2048)
  val msg: Msg = TextMsg(1001, "testMessssage", 6.66f)
  val arr: Array[Byte] = msg.fillMiddleBuffer(sendBuffer).result()
  val strice = BinaryMessage.Strict(ByteString(arr)) // 这个就可以直接通过akka http发送了
  ```
  
* decode  
  ```
  val buffer = new MiddleBufferInJvm(bMsg.asByteBuffer)
  val msg: Msg =
    bytesDecode[Msg](buffer) match {
       case Right(v) => v
       case Left(e) =>
         println(s"decode error: ${e.message}")
         TextMsg(-1, "decode error", 5.555f)
    }
  //msg 就可以直接交给Actor处理了;
  ```
  具体参看backend：com.neo.sk.hiStream.http.ChatService


### In Js
* encode  
  ```
  val sendBuffer = new MiddleBufferInJs(2048)
  val msg: Msg = TextMsg(1001, "testMessssage", 6.66f)
  msg.fillMiddleBuffer(sendBuffer)
  val ab: ArrayBuffer = sendBuffer.result()
  //ab就可以直接用ws发送了，ws.send(ab)
  ```
  
* decode  
  ```
  val blobMsg: Blob = ...
  val fr = new FileReader()
  fr.readAsArrayBuffer(blobMsg)
  fr.onloadend = { _: Event =>
    val buf = fr.result.asInstanceOf[ArrayBuffer]
    val middleDataInJs = new MiddleBufferInJs(buf) // get middle data.
    bytesDecode[Msg](middleDataInJs) match { //decode here.
      case Right(data) => data match {
        case m: TextMsg => ... //process m
        case m: MultiTextMsg => ... //process m
      }
      case Left(error) => println(s"got error: ${error.message}")
    }
  }
  ```
  具体参看frontend：com.neo.sk.hiStream.front.chat.Main
