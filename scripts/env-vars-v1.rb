#!/usr/bin/env ruby

<<EOF
NOTE(AARON):
I think this should be a larger Ruby project.  Not anything official - just a
few scripts is fine.

One part is to find all expected etcdctl keys.  To do this we want to scrape
through coreos-files/services and find all places where `etcdctl' is called. We
can split that line and get the environment variable and the etcdctl key.  Both
of these should be noted in a highly readable list; probably alphanumerically
sorted.

The other part is to generate a complete list of all environment variables used
by the services.  This is fairly simple to do in ruby (see the ruby repos above)
and probably only slightly more complicated for Go or other projects.  The best
thing to do here would be to isolate the specific env var
(ENV['<VAR_NAME_HERE>'] in Ruby) and create a full list, probably sorted
alphanumerically.  We can use the results here and compare with those from part
one.  This will let us determine which env vars we don't actually set anywhere.

Unfortunately, there's some amount of manual intervention involved, because we
have some debug env vars that aren't meant to be set.  That is; we'll get false
positives that we have to filter out.
EOF

def etcdctl_keys_and_env_vars
  <<-EOF
EXAMPLE strings to parse:
services/cronjobs.service:ExecStartPre=/bin/bash -c '/usr/bin/docker pull mogo/cronjobs:`etcdctl get /release/soa`'
services/cronjobs.service:-e REDIS_HOST=`etcdctl get /redis/host` \
services/cronjobs.service:-e REDIS_PORT=`etcdctl get /redis/port` \
services/cronjobs.service:-e REDIS_DB=`etcdctl get /redis/db` \
services/cronjobs.service:-e MQ_HOST=`etcdctl get /rabbitmq/host`
  EOF

  unit_file_env_vars = {}
  File.foreach("/home/aaron/release-notes/unit_file_vars.txt") do |line|
    match_result = /^services\/(\w+)@?.service.*-e (.*)=`etcdctl get (.*)`/.match(line)

    if match_result
      unit_file_env_vars[match_result[2].rstrip.strip] = match_result[3].rstrip.strip
    end
  end

  etcdctl_vars = {}
  File.foreach("/home/aaron/release-notes/etcdctl_values.txt") do |line|
    match_result = /^(.*):(.*)$/.match(line)

    if match_result
      etcdctl_vars[match_result[1].rstrip.strip] = match_result[2].rstrip.strip
    end
  end

  unit_file_env_vars.each do |env, etcdctl|
    if !etcdctl_vars.has_key?(etcdctl)
      puts "Expected etcdctl key `#{etcdctl}' for env var `#{env}'"
    end
  end

  env_vars = {}
  new_service = false
  service = ""
  File.foreach("/home/aaron/release-notes/env_vars.txt") do |line|
    if line.match(/----------/)
      new_service = !new_service
    elsif new_service
      service = line.match(/(\w+):/)[1]
    else
      match_results = /ENV\[(?:'|")(\w+)(?:'|")\]/.match(line)
      if (match_results && match_results.length > 1)
        for i in 1..(match_results.length - 1)
          var = match_results[i]
          if !env_vars.has_key?(var)
            env_vars[var] = [service]
          else
            (env_vars[var] << service).uniq!
          end
        end
      end
    end
  end

  # Print out env vars explicitly used in Ruby source code.
  env_vars.each do |var, services|
    printf("%-45s %s\n", var, services.join(','))
  end

  # Diff explicit env vars against unit files.
  env_vars.each do |var, services|
    if !unit_file_env_vars.has_key?(var)
      puts "Expected unit file env var declaration for `#{var}'"
    end
  end
end

etcdctl_keys_and_env_vars
