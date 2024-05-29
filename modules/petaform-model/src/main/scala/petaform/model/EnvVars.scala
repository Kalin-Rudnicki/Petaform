package petaform.model

final case class EnvVars(
    fromSystem: Map[String, String],
    additional: Map[String, String],
) {

  val map: Map[String, String] = fromSystem ++ additional

  def get(key: String): Option[String] = map.get(key)
  def add(key: String, value: String): EnvVars = EnvVars(fromSystem, additional + (key -> value))
  def addAll(vars: Map[String, String]): EnvVars = EnvVars(fromSystem, additional ++ vars)

}
