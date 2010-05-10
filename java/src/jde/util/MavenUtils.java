/**
 * $Id; $
 *
 * Copyright (C) 2010 by Paul Landes
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package jde.util;

import java.util.Iterator;
import java.util.List;

import org.apache.maven.artifact.ant.DependenciesTask;
import org.apache.maven.model.Dependency;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;


/**
 * Utility class for useful things provided by Maven or Maven Ant Tasks.
 *
 * @author Paul Landes
 */
public class MavenUtils {

	private List<Dependency> createDeps(String[] args) {
		List<Dependency> deps = new java.util.ArrayList<Dependency>();
		for (int i = 0; i < args.length; i += 4) {
			Dependency dep = new Dependency();
			dep.setGroupId(args[i]);
			dep.setArtifactId(args[i + 1]);
			dep.setVersion(args[i + 2]);
			dep.setScope(args[i + 3]);
			deps.add(dep);
		}
		return deps;
	}

	/**
	 * Produce a listing of classpath Maven dependencies.
	 */
	protected String getDependencyPath(String startTok, String endTok, String delim, String... args) {
		return getDependencyPath(startTok, endTok, delim, createDeps(args));
	}


	/**
	 * Produce a listing of classpath Maven dependencies.
	 */
	protected String getDependencyPath(String startTok, String endTok, String delim, List<Dependency> deps) {

		Project prj = new Project();
		DependenciesTask dt = new DependenciesTask();
		Path path = null;
		StringBuilder bld = new StringBuilder(startTok);

		dt.setProject(prj);
		dt.setPathId("pathId");
		for (Dependency dep : deps) dt.addDependency(dep);

		try {
			dt.execute();
			path = (Path)prj.getReference("pathId");
			for(Iterator i = path.iterator(); i.hasNext();) {
				bld.append(i.next());
				if (i.hasNext()) bld.append(delim);
			}
		} catch(Exception e) {
			e.printStackTrace();
			System.exit(1);
		}
		bld.append(endTok);
		return bld.toString();
	}

	public static void main(String[] args) {
		if (((args.length % 4) != 0) || (args.length < 4)) {
			System.err.println("usage: java ... MavenUtils <groupId1> <artifactId1> <version1> <scope1> [<groupId2> <artifactId2> <version2> <scope2>...]");
			System.err.println("example: java ... MavenUtils org.apache.pdfbox pdfbox 1.0.0 compile");
		}
		else {
			System.out.println(new MavenUtils().getDependencyPath("", "", java.io.File.pathSeparator, args));
			System.exit(0);
		}
	}
}
