data BookInfo = Book Int String [String]
                deriving (Show)

bookId (Book id title authors)      = id
bookTitle (Book id title authors)   = title
bookAuthors (Book id title authors) = authors

data Roygbiv = Red
             | Orange
             | Yellow
             | Green
             | Blue
             | Indigo
             | Violet
              deriving (Eq, Show)

