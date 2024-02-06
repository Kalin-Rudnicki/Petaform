
terraform {

  required_providers {
    docker = {
      source = "kreuzwerker/docker"
      version = "3.0.2"
    }
  }

}

provider "docker" {}

resource "docker_volume" "db-data" {
  name = "db-data"
  driver = "local"
  labels {
    label = "application"
    value = "postgres"
  }
}

resource "docker_image" "db" {
  name = "postgres:latest"
  keep_locally = true
}

resource "docker_container" "db2" {
  image = docker_image.db.image_id
  name  = "local-db2"

  env = [
    "POSTGRES_PASSWORD=psql-pass",
    "POSTGRES_DB=example_db",
  ]

  labels {
    label = "application"
    value = "postgres"
  }

  ports {
    internal = 5432
    external = 5000
  }

  mounts {
    target = "/var/lib/postgresql/data"
    source = docker_volume.db-data.name
    read_only = false
    type = "volume"
  }
}
