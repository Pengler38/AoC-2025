module Util

export
partial
unwrap : Maybe a -> a
unwrap (Just v) = v
