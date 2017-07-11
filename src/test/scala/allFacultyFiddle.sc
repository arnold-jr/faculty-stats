
val foo = <span id="ctl00_ContentPlaceHolder1_dlFaculty_ctl00_lblName"
               class="SectionHead">Aaronson, Oran S.</span>

foo match {
  case sp @ <span>{children @ _ *}</span> => (sp \ "@id" text).endsWith("lblName")
  case _ => false
}

List(List("a","b"), List()).flatMap {
  case a :: b :: Nil => Some(b, a)
  case _ => None
}
