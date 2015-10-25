package uk.org.russet;

/**
 * Hello world!
 *
 */
public class App
{
    public static String hello(){
        return "Hello World!";
    }

    public static void crash(){
        throw new RuntimeException("You asked for it");
    }
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
    }
}
