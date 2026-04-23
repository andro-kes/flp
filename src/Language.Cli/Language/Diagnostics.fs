namespace Language

type Error = { Message: string }

exception LangError of Error
