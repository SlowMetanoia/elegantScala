/**
 * value-типы
 * То, что в java называлось бы базовыми типами.
 * Только в scala всё является объектом.
 */
 
var n:Int = 42                //целое
var la:Byte = -7              //знаковый байт
var ch:Char = 'a'             //символ
val str:String = "Well, well" //строка
val x:Double = 2.718281       //число с плавающей точкой
val f:Int=>Int = a => a+1     //функция, принимающая Int и возвращающая Int

/**
 * В scala, также как в java, все значения предаются по ссылке, потому имя переменной стоит воспринимать как название,
 * разрешаемое в адрес.
 * var и val - ключевые слова, обозначающие соответственно, может быть изменена ссылка,
 * в которую разрешается имя переменной или не может.
 */
n = n+1
//str = str + str // ошибка

/**
 * Структуры.
 * Базовые структуры в scala имеют фабричный метод и конструктор.
 * Как правило, в фабричный метод можно передать набор значений, а конструктор принимает Int, задающий размер структуры.
 * В целях лучшей читаемости кода можно указывать тип явно, но чаще всего, в случае когда тип очевиден,
 * компилятор определит его сам. При этом, типизация в scala - статическая.
 */
val arr0 = Array(1,2,3)
val arr1 = new Array[Int](4)
val strs1 = Set(
  "0",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7"
)
val strs2 = Set(
  "0",
  "2",
  "4",
  "7"
)
val c = 0
strs1 diff strs2