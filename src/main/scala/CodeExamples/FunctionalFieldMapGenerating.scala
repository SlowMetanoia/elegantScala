package CodeExamples

import scala.concurrent.Promise
import scala.util.Try

/**
 * Пример - это вполне реальный написанный мной код.
 *
 * В библиотеке scala swing есть штука, которая называется textField - текстовое поле для ввода пользователем.
 * Проблема в том, что при изменении его значения пользователем, необходимо выполнять некоторые действия
 * Я написал небольшую обёртку, позволяющую создавать нужные мне поля короче. Именно она описана ниже.
 * Действия при изменении поля пользователем описываются через se. init - начальное значение поля,
 * а name - подпись сверху поля.
 *
 */
class NumberTextField(init:Double,name:String,se:Double=>Unit){
  def tfValue:Option[Double] = ???
}
object seContainer{
  /**
   * Далее. Этот конкретный код - это кусочек класса создающего 6 взаимосвязанных полей,
   * при изменении любого из значений которых, пересчитывается некоторое значение.
   * Здесь это тоже обозначено как se:
   */
  val se:(Double,Double,Double,Double,Double,Double) => Unit = ???
}

object TryOne{
  import seContainer.se
  
  
  /**
   * Собственно получившийся код
   */
  val fields: Seq[NumberTextField ] = {
    //имена полей
    val names = Seq("scaleX", "shareX", "shiftX", "scaleY", "shareY", "shiftY")
    //связываем имена с начальными значениями и инициализируем promise'ы
    val fields = names
      .zip(Seq(1, 0, 0, 1, 0, 0))
      .map(x => x -> Promise[ NumberTextField ]())
    //завершаем promise'ы
    fields.map{
      case ((name,init),promise) =>
        promise.complete(
          Try(
            //собственно создаём текстовые поля...
            new NumberTextField(init,name, _ => {
              //в вложенной функции вынимаем все значения из promise
              val fieldValues = fields.map(_._2)
                                      .map(promise=> promise
                                        .future
                                        .value
                                        .get
                                        .get
                                        .tfValue
                                           )
              //проверяем, что с ними всё ок и если действительно ок - вызываем se
              if(fieldValues.forall(_.isDefined)) fieldValues.map(_.get) match {
                case Seq(scaleX,shareX,shiftX,scaleY,shareY,shiftY) =>
                  se(scaleX,shareX,shiftX,scaleY,shareY,shiftY)
              }
            }
                                )
            )
          )
        //возвращаем непосредственно текстовые поля, вынимая их из promise
        promise
          .future
          .value
          .get
          .get
    }
  }
  
  /**
   * Ну, громоздко, конечно, но в остальном выглядит относительно сносно: все детали скрыты в одноразовом скоупе,
   * а видимыми остались уже инициализированные обёртки в том виде, в котором нам удобно.
   *
   * Но давайте рассмотрим по подробнее, что же тут вообще происходит:
   * 1. Начнём с главного: как вообще работает se каждого поля?
   * То есть, когда пользователь изменит его значение, программа достанет promise'ы из fields, достанет из них обёртки,
   * из обёрток поля, проверит их и т.д. (начиная со строки 45)
   * Но всё это значит, что fields (то есть вложенное в scope значение) должно остаться в памяти (если ничего не путаю,
   * такое поведение называется замыканием (closure)). Хотя вообще-то,нам нужны только значения полей,
   * а все эти обёртки сверху - хранить излишне. Да и вообще, может статься, что операции в se - менее трудоёмки
   * по сравнению с развёртыванием объектов.
   *
   * 2. Как ни крути, это громоздко. Сам код при наличии комментариев не кажется чересчур сложным, однако боюсь,
   * если их убрать он станет довольно неясным. Это плохой признак.
   *
   * 3. Приходится использовать не совсем красивый трюк с разворачиванием Promise, используя то знание,
   * что к моменту развёртки, все promise уже будут завершены. Это избавляет от
   * использования var, но сам подход делает код более неочевидным.
   *
   * 4. Однако хочется рассмотреть эту задачу и с точки зрения ООП:
   * По большому счёту, нам нужно создать несколько взаимосвязанных объектов. И если вы помните паттерны, то это должно
   * было навести на мысль о довольно редком, тем не менее известном паттерне: абстрактной фабрике.
   * Суть паттерна именно в том, чтобы инкапсулировать создание взаимосвязанных, или взаимозависимых объектов.
   */
}


/**
 * Учитывая всё выше сказанное, сделаем в лоб императивно, закрыв это неприглядное там, где его не будет видно.
 */
object TryTwo{
  import seContainer.se
  val fields = {
    //имена и начальные значения
    val names = Seq("scaleX", "shareX", "shiftX", "scaleY", "shareY", "shiftY")
    val inits = Seq(1,0,0,1,0,0)
    var fields = Seq.empty[NumberTextField]
    //В остальном всё также, но короче и более читаемо.
    fields = names
      .zip(inits)
      .map{
        case (name,init) =>
          new NumberTextField(init,name, _=>{
            val fieldValues = fields
              .map(_.tfValue)
            //проверяем, что с ними всё ок и если действительно ок - вызываем se
            if(fieldValues.forall(_.isDefined)) fieldValues.map(_.get) match {
              case Seq(scaleX,shareX,shiftX,scaleY,shareY,shiftY) =>
                se(scaleX,shareX,shiftX,scaleY,shareY,shiftY)
            }}
                              )
      }
    fields
  }
  /**
   * Итак, что у нас получилось.
   * 1. По логике функционирования всё то-же самое, но практически в 2 раза короче.
   *
   * 2. единственный var спрятан внутри, так что выходное значение можно оставить val.
   *
   * 3. замыкание осталось, но теперь мы не держим лишних обёрток над нужным нам объектом. В целом, это не было критично
   * для производительности, но учитывая, что такое решение и читается лучше - оно предпочтительнее.
   *
   * 4. Абстрактную фабрику здесь я не писал,
   * но это легко сделать заменив val fields на соответствующее объявление метода.
   */
}
/**
 * Вообще не лишним будет сказать, что сама проблема двусторонней зависимости в функциональном программировании
 * чисто функциональными средствами решается грустно. Первое решение написано не очень чисто.
 * Это можно исправить, заменив явное развёртывание операциями надо монадой promise и вложенными обёртками значений,
 * однако это ещё сильнее раздует и без того немаленький код.
 */