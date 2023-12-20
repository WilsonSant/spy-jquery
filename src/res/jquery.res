type documentDom = {
  mutable value: string,
  mutable innerHtml: string,
  mutable innerText: string,
}

type rec customElement = {
  val: string => customElement,
  text: string => customElement,
  html: string => customElement,
}
@val @scope(("window", "document"))
external getElementById: string => documentDom = "getElementById"

type documentJQuery = {on: unit}

let insertIntoElement = (element: documentDom, method: string, value, custom: customElement) => {
  switch method {
  | "Val" => element.value = value
  | "Txt" => element.innerText = value
  | "HTML" => element.innerHtml = value
  | "" => ()
  }
  custom
}

let \"$" = id => {
  let element = getElementById(id)

  let rec custom = {
    val: value => insertIntoElement(element, "Val", value, custom),
    text: value => insertIntoElement(element, "Txt", value, custom),
    html: value => insertIntoElement(element, "HTML", value, custom),
  }
  custom
}
