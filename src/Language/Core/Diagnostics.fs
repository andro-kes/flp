namespace Language.Core

type Error = { Message: string }

exception LangError of Error
