package jdee.maven;

import java.io.File;
import java.util.ArrayList;
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

    public ExecJavaMojo() {
    }

    @Override
    public void execute() throws MojoExecutionException, MojoFailureException {
        System.setProperty("jdee.sourceRoots", asPath(compileSourceRoots));
        System.setProperty("jdee.testSourceRoots", asPath(testCompileSourceRoots));
        if ((null != parent) &&
            (null != parent.getBasedir())) {
            System.out.println("parent: " + parent +
                               ", basedir: " + parent.getBasedir());
            System.setProperty("jdee.parentPath",
                               parent.getBasedir().getAbsolutePath());
        }
        System.setProperty("jdee.childPaths", asPath(childPaths));

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
     * Convert the list into a path
     */
    private String asPathFromFiles(List<File> pathList) {
        System.out.println("asPathFromFiles(" + pathList + ")");
        for (Object file: pathList) {
            System.out.println(" class: " + file.getClass().getName());
        }
        List<String> names = new ArrayList<String>(pathList.size());
        for (File file: pathList) {
            names.add(file.getAbsolutePath());
        }
        return asPath(names);
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

    @Parameter(defaultValue="${project.parent}", readonly=true)
    private MavenProject parent;
    /**
     * Path to child projects
     */
    @Parameter(defaultValue="${project.modules}", readonly=true)
    private List<String> childPaths;
}

