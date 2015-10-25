package jdee.maven;

import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.ResolutionScope;

@Mojo(name="java",threadSafe=true,requiresDependencyResolution=ResolutionScope.TEST)
public class ExecJavaMojo
    extends org.codehaus.mojo.exec.ExecJavaMojo{
    // empty implementation -- we need nothing other than extra dependencies
    // in the project.
}
            
