package jdee.maven;

import java.io.File;
import java.util.List;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

@Mojo(name="java",threadSafe=true,requiresDependencyResolution=ResolutionScope.TEST)
public class ExecJavaMojo
    extends org.codehaus.mojo.exec.ExecJavaMojo

{
    // empty implementation -- we need nothing other than extra dependencies
    // in the project.

    public ExecJavaMojo() {
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        System.setProperty("jdee.sourceRoots", asPath(compileSourceRoots));
        System.setProperty("jdee.testSourceRoots", asPath(testCompileSourceRoots));

        super.execute();
    }

    /**
     * Convert the list into a path
     */
    private String asPath(List<String> pathList) {
        StringBuilder path = new StringBuilder();
        for (String element: pathList) {
            path.append(element).append(File.pathSeparatorChar);
        }
        // Remove file separator character
        if (path.length() > 0) path.deleteCharAt(path.length()-1);

        return path.toString();
    }

    /**
     * The source directories containing the sources to be processed.
     *
     */
    @Parameter(defaultValue="${project.compileSourceRoots}", readonly=true)
    private List<String> compileSourceRoots;

    /**
     * The test source directories containing the sources to be processed.
     *
     */
    @Parameter(defaultValue="${project.testCompileSourceRoots}", readonly=true)
    private List<String> testCompileSourceRoots;


}

