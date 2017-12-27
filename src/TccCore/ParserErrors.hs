module TccCore.ParserErrors where

type ParseError = String

unexpectedEof          = "Unexpected EOF" :: ParseError
missingFuncDeclaration = "Expected function declaration" :: ParseError
missingOpenBrace       = "Expected an opening brace" :: ParseError
missingCloseBrace      = "Expected a closing brace" :: ParseError
missingStatement       = "Expected a valid statement" :: ParseError

