package jde.parser.visitor;
import jde.parser.syntaxtree.*;
import java.util.*;

public class GetVariablesVisitor extends DepthFirstVisitor {



   /**
    * <PRE>
    * f0 -> ( "public" | "protected" | "private" | "static" | "final" | "transient" | "volatile" )*
    * f1 -> Type()
    * f2 -> VariableDeclarator()
    * f3 -> ( "," VariableDeclarator() )*
    * f4 -> ";"
    * </PRE>
    */
   public void visit(FieldDeclaration n) {

     contextFieldDeclaration = true;

     n.f0.accept(this);
     n.f1.accept(this);
     n.f2.accept(this);
     n.f3.accept(this);
     n.f4.accept(this);

     contextFieldDeclaration = false;
     System.out.println(variableId + " " + variableType + " bl=" + beginLine + " bc=" + beginColumn);
     
   }

   /**
    * <PRE>
    * f0 -> VariableDeclaratorId()
    * f1 -> [ "=" VariableInitializer() ]
    * </PRE>
    */
   public void visit(VariableDeclarator n) {
      n.f0.accept(this);
      n.f1.accept(this);
   }

   /**
    * <PRE>
    * f0 -> &lt;IDENTIFIER&gt;
    * f1 -> ( "[" "]" )*
    * </PRE>
    */
   public void visit(VariableDeclaratorId n) {
     
     if (contextFieldDeclaration) {
       NodeToken t = (NodeToken) n.f0;
       variableId = t.tokenImage;
       beginLine = t.beginLine;
       beginColumn = t.beginColumn;
       endLine = t.endLine;
       endColumn = t.endColumn;
     }
     

      n.f0.accept(this);
      n.f1.accept(this);
   }

   /**
    * <PRE>
    * f0 -> ArrayInitializer()
    *       | Expression()
    * </PRE>
    */
   public void visit(VariableInitializer n) {
      n.f0.accept(this);
   }

   /**
    * <PRE>
    * f0 -> "{"
    * f1 -> [ VariableInitializer() ( "," VariableInitializer() )* ]
    * f2 -> [ "," ]
    * f3 -> "}"
    * </PRE>
    */
   public void visit(ArrayInitializer n) {
      n.f0.accept(this);
      n.f1.accept(this);
      n.f2.accept(this);
      n.f3.accept(this);
   }

   /**
    * <PRE>
    * f0 -> [ "final" ]
    * f1 -> Type()
    * f2 -> VariableDeclarator()
    * f3 -> ( "," VariableDeclarator() )*
    * </PRE>
    */
   public void visit(LocalVariableDeclaration n) {
      n.f0.accept(this);
      n.f1.accept(this);
      n.f2.accept(this);
      n.f3.accept(this);
   }

   /**
    * <PRE>
    * f0 -> "boolean"
    *       | "char"
    *       | "byte"
    *       | "short"
    *       | "int"
    *       | "long"
    *       | "float"
    *       | "double"
    * </PRE>
    */
   public void visit(PrimitiveType n) {
     if (contextFieldDeclaration) {
       NodeToken t = (NodeToken) ((NodeChoice) n.f0).choice;
       variableType = t.tokenImage;
       
     }
     
      n.f0.accept(this);
   }

 
   /**
    * <PRE>
    * f0 -> &lt;IDENTIFIER&gt;
    * f1 -> ( "." &lt;IDENTIFIER&gt; )*
    * </PRE>
    */
   public void visit(Name n) {

     if (contextFieldDeclaration) {   
       NodeToken t = (NodeToken) n.f0;
       variableType = t.tokenImage;       
     }
     
      n.f0.accept(this);
      n.f1.accept(this);
   }


  boolean contextFieldDeclaration = false;
  String variableId;
  String variableType;
  int beginLine, beginColumn, endLine, endColumn;  
  
}
