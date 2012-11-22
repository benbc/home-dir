$home = '/home/ben'

define homedir() {
  file {"$home/$name":
    ensure => directory,
    owner => ben,
    group => ben,
  }
}

define project($url) {
  $location = "$home/projects/$name"

  exec {"check-out-$name":
    command => "/usr/bin/sudo -u ben -i /usr/bin/git clone $url $location",
    creates => $location,
    require => [Package['git'], Homedir['projects']],
  }
}

package {['emacs', 'puppet', 'git', 'inotify-tools']:
  ensure => latest,
}

homedir {['work', 'projects']: }

project {['home-dir']: 
  url => 'git@github.com:benbc/home-dir.git',
}

file {"$home/bin":
  ensure => "$home/projects/home-dir/bin",
  require => Project['home-dir'],
}

file {"$home/.gitconfig":
  ensure => "$home/projects/home-dir/.gitconfig",
  require => Project['home-dir'],
}

file {'/mnt/backups':
  ensure => directory,
}

mount {'/mnt/backups':
  device => '/dev/sda4',
  ensure => mounted,
  fstype => auto,
  options => defaults,
  atboot => true,
  require => File['/mnt/backups'],
}
