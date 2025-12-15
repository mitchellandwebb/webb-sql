module Test.Token.TokenSpec where

import Test.Prelude
import Webb.Sql.Query.Token

import Webb.Sql.Query.Token as T

spec :: Spec Unit
spec = describe "Token tests" do

  it "single quoted string" do 
    a <- parse "'abcd'" stringLit
    a.kind === STRING

  it "select" do 
    check "select" SELECT
    check "SELECT" SELECT
    
  it "from" do 
    check "from" FROM
  
  it "where" do 
    check "where" WHERE
    
  it "order" do 
    check "order" ORDER
    
  it "group" do 
    check "group" GROUP
    
  it "by" do
    check "by" BY
    
  it "joins and operators" do
    check "inner" INNER
    check "outer" OUTER
    check "join" JOIN
    check "left" LEFT
    check "right" RIGHT
    check "limit" LIMIT
    check "this" THIS
    check "asc" ASC
    check "desc" DESC
    check "." DOT
    check "&&" AND_OP
    check "||" OR_OP
    check "*" STAR
    check "," COMMA
    check "-" MINUS
    check "+" PLUS
    check "%" MOD
    check "(" LEFT_PAREN
    check ")" RIGHT_PAREN
    check "like" LIKE
    check ">" T.GT
    check "<" T.LT
    check ">=" GTE
    check "<=" LTE
    check "=" EQUAL
    check "!=" NOT_EQUAL
    
  it "doubleq strings" do 
    check "\"abcd\"" STRING
    
  it "singleq strings" do
    check "'abcd'" STRING
    
  it "integer" do
    check "1" NUMBER
    
  it "decimal" do  
    check "123.11" NUMBER

  it "exponent" do  
    check "123.11E10" NUMBER
    
  it "boolean" do 
    check "true" BOOLEAN
    check "false" BOOLEAN
    
  it "more keywords" do 
    check "on" ON
    check "true123" IDENT

    
  where 
  check str token = do
    kinds <- getKinds str
    kinds === [token]

  getKinds str = do
    toks <- tokenize str
    pure $ _.kind <$> toks
