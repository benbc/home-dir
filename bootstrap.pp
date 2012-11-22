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

define ppa($team, $ppa) {
  exec {"add-ppa-$name":
    command => "/usr/bin/add-apt-repository ppa:$team/$ppa",
    creates => "/etc/apt/sources.list.d/${team}-${ppa}-$lsbdistcodename.list",
    notify => Exec['apt-get-update'], 
  }

  exec {"ensure-apt-get-update-is-called-before-ppa-$name-is-used":
    command => '/bin/true',
    subscribe => Exec["add-ppa-$name"],
    require => Exec['apt-get-update'],
    refreshonly => true,
  }
}

exec {'apt-get-update':
  command => '/usr/bin/apt-get update',
  refreshonly => true,
}

ppa {'emacs-snapshots':
  team => 'cassou',
  ppa => 'emacs',
}

package {['puppet', 'puppet-el', 'git', 'inotify-tools', 'xmonad', 'xmobar', 'trayer', 'rxvt-unicode',
          'suckless-tools']:
  ensure => latest,
  require => Exec['apt-get-update'],
}

package {'emacs-snapshot':
  ensure => latest,
  require => Ppa['emacs-snapshots'],
}

homedir {['work', 'projects', '.emacs.d']: }

project {['home-dir']: 
  url => 'git@github.com:benbc/home-dir.git',
}

define vcs-link() {
  file {"$home/$name":
    ensure => "$home/projects/home-dir/$name",
    require => Project['home-dir'],
  }
}

vcs-link{['bin', '.gitconfig', '.emacs.d/init.el']: }

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
