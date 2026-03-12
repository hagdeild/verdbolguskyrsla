library(tidyverse)
library(rvest)
library(chromote)

chrome <- chromote::Chromote$new(
  browser = chromote::Chrome$new(
    args = c(
      "--disable-blink-features=AutomationControlled",
      "--window-size=1920,1080"
    )
  )
)
b <- chrome$new_session()
on.exit({
  b$close()
  chrome$close()
})

b$Runtime$evaluate(
  "Object.defineProperty(navigator, 'webdriver', {get: () => undefined})"
)
b$Page$navigate("https://tradingeconomics.com/bonds")
Sys.sleep(8)

html_text <- b$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value
page <- read_html(html_text)
tables <- page |> html_elements("table")
