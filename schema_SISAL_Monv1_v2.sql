-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema SISAL_Monv1
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema SISAL_Monv1
-- -----------------------------------------------------
DROP SCHEMA IF EXISTS `SISAL_Monv1`;
CREATE SCHEMA IF NOT EXISTS `SISAL_Monv1` DEFAULT CHARACTER SET utf8 ;
USE `SISAL_Monv1` ;

-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`SITE`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`SITE` (
  `site_id` INT NOT NULL AUTO_INCREMENT,
  `site_name` VARCHAR(255) NOT NULL,
  `latitude` DECIMAL(6,4) NOT NULL,
  `longitude` DECIMAL(7,4) NOT NULL,
  `elevation` DECIMAL(10,1) NOT NULL,
  PRIMARY KEY (`site_id`),
  UNIQUE INDEX `site_id_UNIQUE` (`site_id` ASC),
  UNIQUE INDEX `site_name_UNIQUE` (`site_name` ASC))
ENGINE = InnoDB
AUTO_INCREMENT = 1000;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`NOTES`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`NOTES` (
  `site_id` INT NOT NULL,
  `notes` TEXT(8000) NOT NULL,
  PRIMARY KEY (`site_id`),
  INDEX `fk_NOTES_SITE_idx` (`site_id` ASC),
  CONSTRAINT `fk_NOTES_SITE`
    FOREIGN KEY (`site_id`)
    REFERENCES `SISAL_Monv1`.`SITE` (`site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`P_T_PET_AET`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`P_T_PET_AET` (
  `site_id` INT NOT NULL AUTO_INCREMENT,
  `p` VARCHAR(255) NOT NULL,
  `t` DECIMAL(6,4) NOT NULL,
  `pet` DECIMAL(7,4) NOT NULL,
  `aet` DECIMAL(10,1) NOT NULL,
  PRIMARY KEY (`site_id`),
  INDEX `fk_P_T_PET_AET_SITE_idx` (`site_id` ASC),
  CONSTRAINT `fk_P_T_PET_AET_SITE`
    FOREIGN KEY (`site_id`)
    REFERENCES `SISAL_Monv1`.`SITE` (`site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`PRECIP_SITE_METADATA`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`PRECIP_SITE_METADATA` (
  `site_id` INT NOT NULL,
  `precip_site_id` INT NOT NULL AUTO_INCREMENT,
  `precip_site_name` VARCHAR(255) NULL DEFAULT NULL,
  `precip_latitude` DECIMAL(6,4) NULL DEFAULT NULL,
  `precip_longitude` DECIMAL(7,4) NULL DEFAULT NULL,
  `precip_elevation` DECIMAL(10,1) NULL DEFAULT NULL,
  `precip_distance_cave_entrance` DOUBLE NULL DEFAULT NULL,
  PRIMARY KEY (`precip_site_id`),
  INDEX `fk_PRECIP_SITE_METADATA_SITE1_idx` (`site_id` ASC),
  CONSTRAINT `fk_PRECIP_SITE_METADATA_SITE1`
    FOREIGN KEY (`site_id`)
    REFERENCES `SISAL_Monv1`.`SITE` (`site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`PRECIP_ENTITY_METADATA`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`PRECIP_ENTITY_METADATA` (
  `precip_site_id` INT NOT NULL,
  `precip_entity_id` INT NOT NULL AUTO_INCREMENT,
  `precip_entity_name` VARCHAR(255) NULL DEFAULT NULL,
  `precip_method` TEXT(8000) NULL DEFAULT NULL,
  `precip_entity_contact` VARCHAR(255) NULL DEFAULT NULL,
  PRIMARY KEY (`precip_entity_id`),
  INDEX `fk_PRECIP_ENTITY_METADATA_PRECIP_SITE_METADATA1_idx` (`precip_site_id` ASC),
  CONSTRAINT `fk_PRECIP_ENTITY_METADATA_PRECIP_SITE_METADATA1`
    FOREIGN KEY (`precip_site_id`)
    REFERENCES `SISAL_Monv1`.`PRECIP_SITE_METADATA` (`precip_site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`PRECIP`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`PRECIP` (
  `precip_entity_id` INT NOT NULL,
  `precip_sample_id` INT NOT NULL AUTO_INCREMENT,
  `precip_start_yyyy` DOUBLE NULL DEFAULT NULL,
  `precip_start_mm` DOUBLE NULL DEFAULT NULL,
  `precip_start_dd` DOUBLE NULL DEFAULT NULL,
  `precip_start_hhmm` DOUBLE NULL DEFAULT NULL,
  `precip_end_yyyy` DOUBLE NULL DEFAULT NULL,
  `precip_end_mm` DOUBLE NULL DEFAULT NULL,
  `precip_end_dd` DOUBLE NULL DEFAULT NULL,
  `precip_end_hhmm` DOUBLE NULL DEFAULT NULL,
  `precip_accumulation_unit` ENUM('days', 'hours', 'minutes', 'unknown') NULL DEFAULT NULL,
  `precip_accumulation_time` DOUBLE NULL DEFAULT NULL,
  `precip_amount` DOUBLE NULL DEFAULT NULL,
  `precip_d18O_measurement` DOUBLE NULL DEFAULT NULL,
  `precip_d18O_precision` DOUBLE NULL DEFAULT NULL,
  `precip_d2H_measurement` DOUBLE NULL DEFAULT NULL,
  `precip_d2H_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_PRECIP_SAMPLE_PRECIP_ENTITY_METADATA1_idx` (`precip_entity_id` ASC),
  PRIMARY KEY (`precip_sample_id`),
  CONSTRAINT `fk_PRECIP_SAMPLE_PRECIP_ENTITY_METADATA1`
    FOREIGN KEY (`precip_entity_id`)
    REFERENCES `SISAL_Monv1`.`PRECIP_ENTITY_METADATA` (`precip_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`CAVE_ENTITY_METADATA`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`CAVE_ENTITY_METADATA` (
  `site_id` INT NOT NULL,
  `cave_entity_id` INT NOT NULL AUTO_INCREMENT,
  `cave_entity_name` VARCHAR(255) NULL DEFAULT NULL,
  `cave_entity_location` TEXT(8000) NOT NULL,
  `cave_temperature` ENUM('yes', 'no', 'unknown') NULL DEFAULT NULL,
  `cave_temperature_frequency` ENUM('regular', 'sporadic (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `cave_temperature_instrument` ENUM('logger', 'hand-held', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `cave_relative_humidity` ENUM('yes', 'no', 'unknown') NULL DEFAULT NULL,
  `cave_relative_humidity_frequency` ENUM('regular', 'sporadic (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `cave_relative_humidity_instrument` ENUM('logger', 'hand-held', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `cave_pCO2` ENUM('yes', 'no', 'unknown') NULL DEFAULT NULL,
  `cave_pCO2_frequency` ENUM('regular', 'sporadic (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `cave_pCO2_instrument` ENUM('logger', 'hand-held', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `cave_entity_contact` VARCHAR(255) NULL DEFAULT NULL,
  INDEX `fk_CAVE_ENTITY_METADATA_SITE1_idx` (`site_id` ASC),
  PRIMARY KEY (`cave_entity_id`),
  CONSTRAINT `fk_CAVE_ENTITY_METADATA_SITE1`
    FOREIGN KEY (`site_id`)
    REFERENCES `SISAL_Monv1`.`SITE` (`site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`CAVE_TEMPERATURE`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`CAVE_TEMPERATURE` (
  `cave_entity_id` INT NOT NULL,
  `cave_temperature_sample_id` INT NOT NULL AUTO_INCREMENT,
  `cave_temperature_yyyy` DOUBLE NULL DEFAULT NULL,
  `cave_temperature_mm` DOUBLE NULL DEFAULT NULL,
  `cave_temperature_dd` DOUBLE NULL DEFAULT NULL,
  `cave_temperature_hhmm` DOUBLE NULL DEFAULT NULL,
  `cave_temperature_number` DOUBLE NULL DEFAULT NULL,
  `cave_temperature_measurement` DOUBLE NULL DEFAULT NULL,
  `cave_temperature_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_CAVE_TEMPERATURE_CAVE_ENTITY_METADATA1_idx` (`cave_entity_id` ASC),
  PRIMARY KEY (`cave_temperature_sample_id`),
  CONSTRAINT `fk_CAVE_TEMPERATURE_CAVE_ENTITY_METADATA1`
    FOREIGN KEY (`cave_entity_id`)
    REFERENCES `SISAL_Monv1`.`CAVE_ENTITY_METADATA` (`cave_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`CAVE_RELATIVE_HUMIDITY`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`CAVE_RELATIVE_HUMIDITY` (
  `cave_entity_id` INT NOT NULL,
  `cave_relative_humidity_sample_id` INT NOT NULL AUTO_INCREMENT,
  `cave_relative_humidity_yyyy` DOUBLE NULL DEFAULT NULL,
  `cave_relative_humidity_mm` DOUBLE NULL DEFAULT NULL,
  `cave_relative_humidity_dd` DOUBLE NULL DEFAULT NULL,
  `cave_relative_humidity_hhmm` DOUBLE NULL DEFAULT NULL,
  `cave_relative_humidity_number` DOUBLE NULL DEFAULT NULL,
  `cave_relative_humidity_measurement` DOUBLE NULL DEFAULT NULL,
  `cave_relative_humidity_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_CAVE_RELATIVE_HUMIDITY_CAVE_ENTITY_METADATA1_idx` (`cave_entity_id` ASC),
  PRIMARY KEY (`cave_relative_humidity_sample_id`),
  CONSTRAINT `fk_CAVE_RELATIVE_HUMIDITY_CAVE_ENTITY_METADATA1`
    FOREIGN KEY (`cave_entity_id`)
    REFERENCES `SISAL_Monv1`.`CAVE_ENTITY_METADATA` (`cave_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`CAVE_pCO2`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`CAVE_pCO2` (
  `cave_entity_id` INT NOT NULL,
  `cave_pCO2_sample_id` INT NOT NULL AUTO_INCREMENT,
  `cave_pCO2_yyyy` DOUBLE NULL DEFAULT NULL,
  `cave_pCO2_mm` DOUBLE NULL DEFAULT NULL,
  `cave_pCO2_dd` DOUBLE NULL DEFAULT NULL,
  `cave_pCO2_hhmm` DOUBLE NULL DEFAULT NULL,
  `cave_pCO2_number` DOUBLE NULL DEFAULT NULL,
  `cave_pCO2_measurement` DOUBLE NULL DEFAULT NULL,
  `cave_pCO2_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_CAVE_pCO2_CAVE_ENTITY_METADATA1_idx` (`cave_entity_id` ASC),
  PRIMARY KEY (`cave_pCO2_sample_id`),
  CONSTRAINT `fk_CAVE_pCO2_CAVE_ENTITY_METADATA1`
    FOREIGN KEY (`cave_entity_id`)
    REFERENCES `SISAL_Monv1`.`CAVE_ENTITY_METADATA` (`cave_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`DRIP_ENTITY_METADATA`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`DRIP_ENTITY_METADATA` (
  `site_id` INT NOT NULL,
  `drip_entity_id` INT NOT NULL AUTO_INCREMENT,
  `drip_entity_name` VARCHAR(255) NULL DEFAULT NULL,
  `entity_id` INT NULL DEFAULT NULL,
  `geology` ENUM('limestone', 'dolomite', 'marble', 'dolomite limestone', 'marly limestone', 'calcarenite', 'mixed (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `rock_age` ENUM('Holocene', 'Pleistocene', 'Pliocene', 'Miocene', 'Oligocene', 'Eocene', 'Palaeocene', 'Cretaceous', 'Jurassic', 'Triassic', 'Permian', 'Carboniferous', 'Devonian', 'Silurian', 'Ordovician', 'Cambrian', 'Precambrian', 'mixed (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `drip_entity_location` TEXT(8000) NOT NULL,
  `drip_iso` ENUM('yes', 'no', 'unknown') NULL DEFAULT NULL,
  `drip_iso_method` TEXT(8000) NULL DEFAULT NULL,
  `drip_rate` ENUM('yes', 'no', 'unknown') NULL DEFAULT NULL,
  `drip_rate_frequency` ENUM('regular', 'sporadic (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `drip_rate_instrument` ENUM('logger', 'hand-held', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `mod_carb` ENUM('yes', 'no', 'unknown') NULL DEFAULT NULL,
  `mod_carb_method` TEXT(8000) NULL DEFAULT NULL,
  `drip_entity_contact` VARCHAR(255) NULL DEFAULT NULL,
  INDEX `fk_DRIP_ENTITY_METADATA_SITE1_idx` (`site_id` ASC),
  PRIMARY KEY (`drip_entity_id`),
  CONSTRAINT `fk_DRIP_ENTITY_METADATA_SITE1`
    FOREIGN KEY (`site_id`)
    REFERENCES `SISAL_Monv1`.`SITE` (`site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`DRIP_ISO`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`DRIP_ISO` (
  `drip_entity_id` INT NOT NULL,
  `drip_iso_sample_id` INT NOT NULL AUTO_INCREMENT,
  `drip_iso_start_yyyy` DOUBLE NULL DEFAULT NULL,
  `drip_iso_start_mm` DOUBLE NULL DEFAULT NULL,
  `drip_iso_start_dd` DOUBLE NULL DEFAULT NULL,
  `drip_iso_start_hhmm` DOUBLE NULL DEFAULT NULL,
  `drip_iso_end_yyyy` DOUBLE NULL DEFAULT NULL,
  `drip_iso_end_mm` DOUBLE NULL DEFAULT NULL,
  `drip_iso_end_dd` DOUBLE NULL DEFAULT NULL,
  `drip_iso_end_hhmm` DOUBLE NULL DEFAULT NULL,
  `drip_iso_accumulation_unit` ENUM('days', 'hours', 'minutes', 'seconds', 'unknown') NULL DEFAULT NULL,
  `drip_iso_accumulation_time` DOUBLE NULL DEFAULT NULL,
  `drip_iso_d18O_measurement` DOUBLE NULL DEFAULT NULL,
  `drip_iso_d18O_precision` DOUBLE NULL DEFAULT NULL,
  `drip_iso_d2H_measurement` DOUBLE NULL DEFAULT NULL,
  `drip_iso_d2H_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_DRIP_ISO_SAMPLE_DRIP_ENTITY_METADATA1_idx` (`drip_entity_id` ASC),
  PRIMARY KEY (`drip_iso_sample_id`),
  CONSTRAINT `fk_DRIP_ISO_SAMPLE_DRIP_ENTITY_METADATA1`
    FOREIGN KEY (`drip_entity_id`)
    REFERENCES `SISAL_Monv1`.`DRIP_ENTITY_METADATA` (`drip_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`DRIP_RATE`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`DRIP_RATE` (
  `drip_entity_id` INT NOT NULL,
  `drip_rate_sample_id` INT NOT NULL AUTO_INCREMENT,
  `drip_rate_start_yyyy` DOUBLE NULL DEFAULT NULL,
  `drip_rate_start_mm` DOUBLE NULL DEFAULT NULL,
  `drip_rate_start_dd` DOUBLE NULL DEFAULT NULL,
  `drip_rate_start_hhmm` DOUBLE NULL DEFAULT NULL,
  `drip_rate_end_yyyy` DOUBLE NULL DEFAULT NULL,
  `drip_rate_end_mm` DOUBLE NULL DEFAULT NULL,
  `drip_rate_end_dd` DOUBLE NULL DEFAULT NULL,
  `drip_rate_end_hhmm` DOUBLE NULL DEFAULT NULL,
  `drip_rate_accumulation_unit` ENUM('days', 'hours', 'minutes', 'seconds', 'unknown') NULL DEFAULT NULL,
  `drip_rate_accumulation_time` DOUBLE NULL DEFAULT NULL,
  `drip_rate_measurement` DOUBLE NULL DEFAULT NULL,
  `drip_rate_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_DRIP_RATE_DRIP_ENTITY_METADATA1_idx` (`drip_entity_id` ASC),
  PRIMARY KEY (`drip_rate_sample_id`),
  CONSTRAINT `fk_DRIP_RATE_DRIP_ENTITY_METADATA1`
    FOREIGN KEY (`drip_entity_id`)
    REFERENCES `SISAL_Monv1`.`DRIP_ENTITY_METADATA` (`drip_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`MOD_CARB`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`MOD_CARB` (
  `drip_entity_id` INT NOT NULL,
  `mod_carb_sample_id` INT NOT NULL AUTO_INCREMENT,
  `mod_carb_start_yyyy` DOUBLE NULL DEFAULT NULL,
  `mod_carb_start_mm` DOUBLE NULL DEFAULT NULL,
  `mod_carb_start_dd` DOUBLE NULL DEFAULT NULL,
  `mod_carb_end_yyyy` DOUBLE NULL DEFAULT NULL,
  `mod_carb_end_mm` DOUBLE NULL DEFAULT NULL,
  `mod_carb_end_dd` DOUBLE NULL DEFAULT NULL,
  `mod_carb_accumulation_unit` ENUM('years', 'months', 'days', 'unknown') NULL DEFAULT NULL,
  `mod_carb_accumulation_time` DOUBLE NULL DEFAULT NULL,
  `mod_carb_surface` ENUM('stalagmite', 'stalagmite scar', 'glass plate', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `mod_carb_mineralogy` ENUM('calcite', 'aragonite', 'mixed (see notes)', 'other (see notes)', 'unknown') NULL DEFAULT NULL,
  `mod_carb_d18O_measurement` DOUBLE NULL DEFAULT NULL,
  `mod_carb_d18O_precision` DOUBLE NULL DEFAULT NULL,
  `mod_carb_d13C_measurement` DOUBLE NULL DEFAULT NULL,
  `mod_carb_d13C_precision` DOUBLE NULL DEFAULT NULL,
  INDEX `fk_MOD_CARB_SAMPLE_DRIP_ENTITY_METADATA1_idx` (`drip_entity_id` ASC),
  PRIMARY KEY (`mod_carb_sample_id`),
  CONSTRAINT `fk_MOD_CARB_SAMPLE_DRIP_ENTITY_METADATA1`
    FOREIGN KEY (`drip_entity_id`)
    REFERENCES `SISAL_Monv1`.`DRIP_ENTITY_METADATA` (`drip_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`REFERENCE`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`REFERENCE` (
  `site_id` INT NOT NULL,
  `ref_id` INT NOT NULL AUTO_INCREMENT,
  `citation` VARCHAR(2555) NULL DEFAULT NULL,
  `publication_DOI` VARCHAR(255) NULL DEFAULT NULL,
  PRIMARY KEY (`ref_id`),
  INDEX `fk_REFERENCE_SITE1_idx` (`site_id` ASC),
  CONSTRAINT `fk_REFERENCE_SITE1`
    FOREIGN KEY (`site_id`)
    REFERENCES `SISAL_Monv1`.`SITE` (`site_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `SISAL_Monv1`.`ENTITY_LINK_REFERENCE`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `SISAL_Monv1`.`ENTITY_LINK_REFERENCE` (
  `precip_entity_id` INT NOT NULL,
  `cave_entity_id` INT NOT NULL,
  `drip_entity_id` INT NOT NULL,
  `ref_id` INT NOT NULL,
  INDEX `fk_ENTITY_LINK_REFERENCE_PRECIP_ENTITY_METADATA1_idx` (`precip_entity_id` ASC),
  INDEX `fk_ENTITY_LINK_REFERENCE_CAVE_ENTITY_METADATA1_idx` (`cave_entity_id` ASC),
  INDEX `fk_ENTITY_LINK_REFERENCE_DRIP_ENTITY_METADATA1_idx` (`drip_entity_id` ASC),
  INDEX `fk_ENTITY_LINK_REFERENCE_REFERENCE1_idx` (`ref_id` ASC),
  CONSTRAINT `fk_ENTITY_LINK_REFERENCE_PRECIP_ENTITY_METADATA1`
    FOREIGN KEY (`precip_entity_id`)
    REFERENCES `SISAL_Monv1`.`PRECIP_ENTITY_METADATA` (`precip_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_ENTITY_LINK_REFERENCE_CAVE_ENTITY_METADATA1`
    FOREIGN KEY (`cave_entity_id`)
    REFERENCES `SISAL_Monv1`.`CAVE_ENTITY_METADATA` (`cave_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_ENTITY_LINK_REFERENCE_DRIP_ENTITY_METADATA1`
    FOREIGN KEY (`drip_entity_id`)
    REFERENCES `SISAL_Monv1`.`DRIP_ENTITY_METADATA` (`drip_entity_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_ENTITY_LINK_REFERENCE_REFERENCE1`
    FOREIGN KEY (`ref_id`)
    REFERENCES `SISAL_Monv1`.`REFERENCE` (`ref_id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
