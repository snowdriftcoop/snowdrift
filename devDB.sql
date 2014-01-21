--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: log_doc_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_development
--

CREATE FUNCTION log_doc_event_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN
            INSERT INTO doc_event (time, doc, blessed_version) SELECT now(), NEW.id, NEW.current_version;
            RETURN NEW;
        END IF;
        RETURN NULL;
    END;
$$;


ALTER FUNCTION public.log_doc_event_trigger() OWNER TO snowdrift_development;

--
-- Name: log_role_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_development
--

CREATE FUNCTION log_role_event_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    BEGIN
        IF (TG_OP = 'DELETE') THEN
            INSERT INTO role_event (time, "user", role, project, added) SELECT now(), OLD."user", OLD.role, OLD.project, 'f';
            RETURN OLD;
        ELSIF (TG_OP = 'INSERT') THEN
            INSERT INTO role_event (time, "user", role, project, added) SELECT now(), NEW."user", NEW.role, NEW.project, 't';
            RETURN NEW;
        END IF;
        RETURN NULL;
    END;
$$;


ALTER FUNCTION public.log_role_event_trigger() OWNER TO snowdrift_development;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: account; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE account (
    id integer NOT NULL,
    balance bigint NOT NULL
);


ALTER TABLE public.account OWNER TO snowdrift_development;

--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.account_id_seq OWNER TO snowdrift_development;

--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE account_id_seq OWNED BY account.id;


--
-- Name: build; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE build (
    id integer NOT NULL,
    boot_time timestamp without time zone NOT NULL,
    base character varying NOT NULL,
    diff character varying NOT NULL
);


ALTER TABLE public.build OWNER TO snowdrift_development;

--
-- Name: build_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE build_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.build_id_seq OWNER TO snowdrift_development;

--
-- Name: build_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE build_id_seq OWNED BY build.id;


--
-- Name: comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    moderated_ts timestamp without time zone,
    moderated_by bigint,
    parent bigint,
    "user" bigint NOT NULL,
    text character varying NOT NULL,
    depth bigint NOT NULL
);


ALTER TABLE public.comment OWNER TO snowdrift_development;

--
-- Name: comment_ancestor; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_ancestor (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ancestor bigint NOT NULL
);


ALTER TABLE public.comment_ancestor OWNER TO snowdrift_development;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_ancestor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_ancestor_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;


--
-- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_id_seq OWNED BY comment.id;


--
-- Name: comment_retraction; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_retraction (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.comment_retraction OWNER TO snowdrift_development;

--
-- Name: comment_retraction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_retraction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_retraction_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_retraction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_retraction.id;


--
-- Name: comment_tag; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE comment_tag (
    id integer NOT NULL,
    comment bigint NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    count bigint DEFAULT 1 NOT NULL
);


ALTER TABLE public.comment_tag OWNER TO snowdrift_development;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE comment_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_tag_id_seq OWNER TO snowdrift_development;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE comment_tag_id_seq OWNED BY comment_tag.id;


--
-- Name: committee_user; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE committee_user (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL
);


ALTER TABLE public.committee_user OWNER TO snowdrift_development;

--
-- Name: committee_user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE committee_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.committee_user_id_seq OWNER TO snowdrift_development;

--
-- Name: committee_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE committee_user_id_seq OWNED BY committee_user.id;


--
-- Name: database_version; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE database_version (
    id integer NOT NULL,
    last_migration bigint NOT NULL
);


ALTER TABLE public.database_version OWNER TO snowdrift_development;

--
-- Name: database_version_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE database_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.database_version_id_seq OWNER TO snowdrift_development;

--
-- Name: database_version_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE database_version_id_seq OWNED BY database_version.id;


--
-- Name: default_tag_color; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE default_tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.default_tag_color OWNER TO snowdrift_development;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE default_tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.default_tag_color_id_seq OWNER TO snowdrift_development;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE default_tag_color_id_seq OWNED BY default_tag_color.id;


--
-- Name: doc; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE doc (
    id integer NOT NULL,
    name character varying NOT NULL,
    current_version bigint NOT NULL
);


ALTER TABLE public.doc OWNER TO snowdrift_development;

--
-- Name: doc_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE doc_event (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    doc bigint NOT NULL,
    blessed_version bigint NOT NULL
);


ALTER TABLE public.doc_event OWNER TO snowdrift_development;

--
-- Name: doc_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE doc_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_event_id_seq OWNER TO snowdrift_development;

--
-- Name: doc_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE doc_event_id_seq OWNED BY doc_event.id;


--
-- Name: doc_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE doc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_id_seq OWNER TO snowdrift_development;

--
-- Name: doc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE doc_id_seq OWNED BY doc.id;


--
-- Name: interest; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE interest (
    id integer NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.interest OWNER TO snowdrift_development;

--
-- Name: interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.interest_id_seq OWNER TO snowdrift_development;

--
-- Name: interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE interest_id_seq OWNED BY interest.id;


--
-- Name: invite; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE invite (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    code character varying NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    tag character varying NOT NULL,
    redeemed boolean NOT NULL,
    redeemed_ts timestamp without time zone,
    redeemed_by bigint
);


ALTER TABLE public.invite OWNER TO snowdrift_development;

--
-- Name: invite_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE invite_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.invite_id_seq OWNER TO snowdrift_development;

--
-- Name: invite_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE invite_id_seq OWNED BY invite.id;


--
-- Name: manual_establishment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE manual_establishment (
    id integer NOT NULL,
    established_user character varying NOT NULL,
    establishing_user character varying NOT NULL
);


ALTER TABLE public.manual_establishment OWNER TO snowdrift_development;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE manual_establishment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.manual_establishment_id_seq OWNER TO snowdrift_development;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE manual_establishment_id_seq OWNED BY manual_establishment.id;


--
-- Name: message; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE message (
    id integer NOT NULL,
    project bigint,
    created_ts timestamp without time zone NOT NULL,
    "from" bigint,
    "to" bigint,
    content character varying NOT NULL
);


ALTER TABLE public.message OWNER TO snowdrift_development;

--
-- Name: message_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.message_id_seq OWNER TO snowdrift_development;

--
-- Name: message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE message_id_seq OWNED BY message.id;


--
-- Name: payday; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE payday (
    id integer NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.payday OWNER TO snowdrift_development;

--
-- Name: payday_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE payday_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.payday_id_seq OWNER TO snowdrift_development;

--
-- Name: payday_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE payday_id_seq OWNED BY payday.id;


--
-- Name: pledge; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE pledge (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL,
    funded_shares bigint NOT NULL
);


ALTER TABLE public.pledge OWNER TO snowdrift_development;

--
-- Name: pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pledge_id_seq OWNER TO snowdrift_development;

--
-- Name: pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE pledge_id_seq OWNED BY pledge.id;


--
-- Name: project; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    name character varying NOT NULL,
    handle character varying NOT NULL,
    description character varying NOT NULL,
    account bigint NOT NULL,
    share_value bigint NOT NULL,
    last_payday bigint,
    github_repo character varying
);


ALTER TABLE public.project OWNER TO snowdrift_development;

--
-- Name: project_blog; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_blog (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    title character varying NOT NULL,
    "user" bigint NOT NULL,
    content character varying NOT NULL
);


ALTER TABLE public.project_blog OWNER TO snowdrift_development;

--
-- Name: project_blog_comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_blog_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    blog bigint NOT NULL
);


ALTER TABLE public.project_blog_comment OWNER TO snowdrift_development;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_blog_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_comment_id_seq OWNER TO snowdrift_development;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_blog_comment_id_seq OWNED BY project_blog_comment.id;


--
-- Name: project_blog_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_blog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_id_seq OWNER TO snowdrift_development;

--
-- Name: project_blog_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_blog_id_seq OWNED BY project_blog.id;


--
-- Name: project_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_id_seq OWNER TO snowdrift_development;

--
-- Name: project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_id_seq OWNED BY project.id;


--
-- Name: project_last_update; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_last_update (
    id integer NOT NULL,
    project bigint NOT NULL,
    update bigint NOT NULL
);


ALTER TABLE public.project_last_update OWNER TO snowdrift_development;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_last_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_last_update_id_seq OWNER TO snowdrift_development;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_last_update_id_seq OWNED BY project_last_update.id;


--
-- Name: project_tag; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_tag (
    id integer NOT NULL,
    project bigint NOT NULL,
    tag bigint NOT NULL
);


ALTER TABLE public.project_tag OWNER TO snowdrift_development;

--
-- Name: project_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_tag_id_seq OWNER TO snowdrift_development;

--
-- Name: project_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_tag_id_seq OWNED BY project_tag.id;


--
-- Name: project_update; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_update (
    id integer NOT NULL,
    updated_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    author bigint NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.project_update OWNER TO snowdrift_development;

--
-- Name: project_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_update_id_seq OWNER TO snowdrift_development;

--
-- Name: project_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_update_id_seq OWNED BY project_update.id;


--
-- Name: project_user_role; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE project_user_role (
    id integer NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL
);


ALTER TABLE public.project_user_role OWNER TO snowdrift_development;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE project_user_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_user_role_id_seq OWNER TO snowdrift_development;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;


--
-- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE role_event (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    project bigint NOT NULL,
    added boolean NOT NULL
);


ALTER TABLE public.role_event OWNER TO snowdrift_development;

--
-- Name: role_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE role_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.role_event_id_seq OWNER TO snowdrift_development;

--
-- Name: role_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE role_event_id_seq OWNED BY role_event.id;


--
-- Name: tag; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE tag (
    id integer NOT NULL,
    name character varying NOT NULL
);


ALTER TABLE public.tag OWNER TO snowdrift_development;

--
-- Name: tag_color; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.tag_color OWNER TO snowdrift_development;

--
-- Name: tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_color_id_seq OWNER TO snowdrift_development;

--
-- Name: tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE tag_color_id_seq OWNED BY tag_color.id;


--
-- Name: tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_id_seq OWNER TO snowdrift_development;

--
-- Name: tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE tag_id_seq OWNED BY tag.id;


--
-- Name: ticket; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE ticket (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    name character varying NOT NULL,
    comment bigint NOT NULL,
    updated_ts timestamp without time zone NOT NULL
);


ALTER TABLE public.ticket OWNER TO snowdrift_development;

--
-- Name: ticket_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE ticket_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ticket_id_seq OWNER TO snowdrift_development;

--
-- Name: ticket_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE ticket_id_seq OWNED BY ticket.id;


--
-- Name: transaction; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE transaction (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    credit bigint,
    debit bigint,
    amount bigint NOT NULL,
    reason character varying NOT NULL,
    info character varying,
    payday bigint
);


ALTER TABLE public.transaction OWNER TO snowdrift_development;

--
-- Name: transaction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.transaction_id_seq OWNER TO snowdrift_development;

--
-- Name: transaction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE transaction_id_seq OWNED BY transaction.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE "user" (
    id integer NOT NULL,
    ident character varying NOT NULL,
    hash character varying,
    salt character varying,
    name character varying,
    account bigint NOT NULL,
    avatar character varying,
    blurb character varying,
    statement character varying,
    irc_nick character varying,
    read_messages timestamp without time zone DEFAULT now() NOT NULL,
    read_applications timestamp without time zone DEFAULT now() NOT NULL,
    read_comments timestamp without time zone DEFAULT now() NOT NULL,
    read_edits timestamp without time zone DEFAULT now() NOT NULL,
    established_ts timestamp without time zone,
    established_reason character varying
);


ALTER TABLE public."user" OWNER TO snowdrift_development;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_id_seq OWNER TO snowdrift_development;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: user_setting; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE user_setting (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    setting character varying NOT NULL,
    value character varying NOT NULL
);


ALTER TABLE public.user_setting OWNER TO snowdrift_development;

--
-- Name: user_setting_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE user_setting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_setting_id_seq OWNER TO snowdrift_development;

--
-- Name: user_setting_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE user_setting_id_seq OWNED BY user_setting.id;


--
-- Name: volunteer_application; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE volunteer_application (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    name character varying NOT NULL,
    email character varying NOT NULL,
    other_contact_info character varying,
    website character varying,
    location character varying,
    experience character varying,
    comments character varying
);


ALTER TABLE public.volunteer_application OWNER TO snowdrift_development;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE volunteer_application_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_application_id_seq OWNER TO snowdrift_development;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE volunteer_application_id_seq OWNED BY volunteer_application.id;


--
-- Name: volunteer_interest; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE volunteer_interest (
    id integer NOT NULL,
    volunteer bigint NOT NULL,
    interest bigint NOT NULL
);


ALTER TABLE public.volunteer_interest OWNER TO snowdrift_development;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE volunteer_interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_interest_id_seq OWNER TO snowdrift_development;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE volunteer_interest_id_seq OWNED BY volunteer_interest.id;


--
-- Name: wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_edit (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    page bigint NOT NULL,
    content character varying NOT NULL,
    comment character varying
);


ALTER TABLE public.wiki_edit OWNER TO snowdrift_development;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_edit_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_edit_id_seq OWNED BY wiki_edit.id;


--
-- Name: wiki_last_edit; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_last_edit (
    id integer NOT NULL,
    page bigint NOT NULL,
    edit bigint NOT NULL
);


ALTER TABLE public.wiki_last_edit OWNER TO snowdrift_development;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_last_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_last_edit_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_last_edit_id_seq OWNED BY wiki_last_edit.id;


--
-- Name: wiki_page; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_page (
    id integer NOT NULL,
    target character varying NOT NULL,
    project bigint NOT NULL,
    content character varying NOT NULL,
    permission_level character varying NOT NULL
);


ALTER TABLE public.wiki_page OWNER TO snowdrift_development;

--
-- Name: wiki_page_comment; Type: TABLE; Schema: public; Owner: snowdrift_development; Tablespace: 
--

CREATE TABLE wiki_page_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    page bigint NOT NULL
);


ALTER TABLE public.wiki_page_comment OWNER TO snowdrift_development;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_page_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_comment_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_page_comment_id_seq OWNED BY wiki_page_comment.id;


--
-- Name: wiki_page_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_development
--

CREATE SEQUENCE wiki_page_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_id_seq OWNER TO snowdrift_development;

--
-- Name: wiki_page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_development
--

ALTER SEQUENCE wiki_page_id_seq OWNED BY wiki_page.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY account ALTER COLUMN id SET DEFAULT nextval('account_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY build ALTER COLUMN id SET DEFAULT nextval('build_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_ancestor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag ALTER COLUMN id SET DEFAULT nextval('comment_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY committee_user ALTER COLUMN id SET DEFAULT nextval('committee_user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY database_version ALTER COLUMN id SET DEFAULT nextval('database_version_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY default_tag_color ALTER COLUMN id SET DEFAULT nextval('default_tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc ALTER COLUMN id SET DEFAULT nextval('doc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc_event ALTER COLUMN id SET DEFAULT nextval('doc_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY interest ALTER COLUMN id SET DEFAULT nextval('interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite ALTER COLUMN id SET DEFAULT nextval('invite_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY manual_establishment ALTER COLUMN id SET DEFAULT nextval('manual_establishment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY message ALTER COLUMN id SET DEFAULT nextval('message_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY payday ALTER COLUMN id SET DEFAULT nextval('payday_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge ALTER COLUMN id SET DEFAULT nextval('pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project ALTER COLUMN id SET DEFAULT nextval('project_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog ALTER COLUMN id SET DEFAULT nextval('project_blog_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog_comment ALTER COLUMN id SET DEFAULT nextval('project_blog_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_last_update ALTER COLUMN id SET DEFAULT nextval('project_last_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_tag ALTER COLUMN id SET DEFAULT nextval('project_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_update ALTER COLUMN id SET DEFAULT nextval('project_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_user_role_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag ALTER COLUMN id SET DEFAULT nextval('tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag_color ALTER COLUMN id SET DEFAULT nextval('tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY ticket ALTER COLUMN id SET DEFAULT nextval('ticket_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction ALTER COLUMN id SET DEFAULT nextval('transaction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_setting ALTER COLUMN id SET DEFAULT nextval('user_setting_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_application ALTER COLUMN id SET DEFAULT nextval('volunteer_application_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_interest ALTER COLUMN id SET DEFAULT nextval('volunteer_interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_edit ALTER COLUMN id SET DEFAULT nextval('wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_last_edit ALTER COLUMN id SET DEFAULT nextval('wiki_last_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page ALTER COLUMN id SET DEFAULT nextval('wiki_page_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page_comment ALTER COLUMN id SET DEFAULT nextval('wiki_page_comment_id_seq'::regclass);


--
-- Data for Name: account; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY account (id, balance) FROM stdin;
1	0
2	0
\.


--
-- Name: account_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('account_id_seq', 2, true);


--
-- Data for Name: build; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY build (id, boot_time, base, diff) FROM stdin;
1	2013-11-20 02:25:05.013214	8604247d8116396ec0214ba7677d2212990517d2	
2	2013-11-23 19:29:20.349772	8604247d8116396ec0214ba7677d2212990517d2	
3	2014-01-20 19:09:52.001477	4ccd10f42fd6d64ac4e2b3d70b28c9c6a3223b0c	diff --git a/Application.hs b/Application.hs\nindex 241f582..bf838a7 100644\n--- a/Application.hs\n+++ b/Application.hs\n@@ -135,8 +135,8 @@ doMigration = do\n \n     mapM_ ((\\ file -> liftIO (putStrLn ("running " ++ file ++ "...") >> T.readFile file)) >=> flip rawExecute []) $ L.map (("migrations/" <>) . snd) migration_files\n \n-    let last_migration = L.maximum $ 0 : L.map fst migration_files\n-    update $ flip set [ DatabaseVersionLastMigration =. val last_migration ]\n+    let new_last_migration = L.maximum $ 0 : L.map fst migration_files\n+    update $ flip set [ DatabaseVersionLastMigration =. val new_last_migration ]\n \n     migrations <- parseMigration' migrateAll\n \n@@ -145,8 +145,10 @@ doMigration = do\n     liftIO $ putStrLn $ "safe: " ++ show (L.length safe)\n     liftIO $ putStrLn $ "unsafe: " ++ show (L.length unsafe)\n \n+    liftIO $ putStrLn $ "new last_migration: " ++ show new_last_migration\n+\n     when (not $ L.null $ L.map snd safe) $ do\n-        liftIO $ T.writeFile ("migrations/migrate" <> show (last_migration + 1)) $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n+        liftIO $ T.writeFile ("migrations/migrate" <> show (new_last_migration + 1)) $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n         mapM_ (flip rawExecute [] . snd) migrations\n \n     when (not $ L.null $ L.map snd unsafe) $ do\n
4	2014-01-21 17:41:12.047799	ed014b5810941e61f82123f97be0c69600d99f0f	
\.


--
-- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('build_id_seq', 4, true);


--
-- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth) FROM stdin;
1	2014-01-21 18:11:03.914397	2014-01-21 18:12:36.696658	1	\N	1	This is a comment.	0
2	2014-01-21 18:13:00.273315	2014-01-21 18:13:10.464805	1	1	1	Replies are threaded.	1
3	2014-01-21 18:13:57.732222	\N	\N	\N	1	When a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.	0
4	2014-01-21 18:15:30.945499	2014-01-21 18:15:37.484472	1	\N	1	adding a line starting with "ticket:" such as\n\nticket: this is a ticket\n\nmakes the post show up at /t where all the tickets are listed	0
\.


--
-- Data for Name: comment_ancestor; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_ancestor (id, comment, ancestor) FROM stdin;
1	2	1
\.


--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);


--
-- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_id_seq', 4, true);


--
-- Data for Name: comment_retraction; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_retraction (id, ts, reason, comment) FROM stdin;
\.


--
-- Name: comment_retraction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_retraction_id_seq', 1, false);


--
-- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY comment_tag (id, comment, tag, "user", count) FROM stdin;
\.


--
-- Name: comment_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('comment_tag_id_seq', 1, false);


--
-- Data for Name: committee_user; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY committee_user (id, created_ts, "user", project) FROM stdin;
\.


--
-- Name: committee_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('committee_user_id_seq', 1, false);


--
-- Data for Name: database_version; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY database_version (id, last_migration) FROM stdin;
1	1
\.


--
-- Name: database_version_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('database_version_id_seq', 1, true);


--
-- Data for Name: default_tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY default_tag_color (id, tag, color) FROM stdin;
\.


--
-- Name: default_tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('default_tag_color_id_seq', 1, false);


--
-- Data for Name: doc; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY doc (id, name, current_version) FROM stdin;
\.


--
-- Data for Name: doc_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY doc_event (id, "time", doc, blessed_version) FROM stdin;
\.


--
-- Name: doc_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('doc_event_id_seq', 1, false);


--
-- Name: doc_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('doc_id_seq', 1, false);


--
-- Data for Name: interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY interest (id, description) FROM stdin;
\.


--
-- Name: interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('interest_id_seq', 1, false);


--
-- Data for Name: invite; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY invite (id, created_ts, project, code, "user", role, tag, redeemed, redeemed_ts, redeemed_by) FROM stdin;
1	2014-01-21 18:12:09.148007	1	df0176d67f1a4063	1	Moderator	admin as also moderator	t	2014-01-21 18:12:24.376052	1
\.


--
-- Name: invite_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('invite_id_seq', 1, true);


--
-- Data for Name: manual_establishment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY manual_establishment (id, established_user, establishing_user) FROM stdin;
\.


--
-- Name: manual_establishment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);


--
-- Data for Name: message; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY message (id, project, created_ts, "from", "to", content) FROM stdin;
\.


--
-- Name: message_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('message_id_seq', 1, false);


--
-- Data for Name: payday; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY payday (id, date) FROM stdin;
\.


--
-- Name: payday_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('payday_id_seq', 1, false);


--
-- Data for Name: pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY pledge (id, "user", project, shares, funded_shares) FROM stdin;
\.


--
-- Name: pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('pledge_id_seq', 1, false);


--
-- Data for Name: project; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project (id, created_ts, name, handle, description, account, share_value, last_payday, github_repo) FROM stdin;
1	2013-11-23 11:52:54.632763	Snowdrift	snowdrift	Snowdrift Project	2	0	\N	\N
\.


--
-- Data for Name: project_blog; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_blog (id, "time", title, "user", content) FROM stdin;
\.


--
-- Data for Name: project_blog_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_blog_comment (id, comment, blog) FROM stdin;
\.


--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_blog_comment_id_seq', 1, false);


--
-- Name: project_blog_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_blog_id_seq', 1, false);


--
-- Name: project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_id_seq', 1, true);


--
-- Data for Name: project_last_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_last_update (id, project, update) FROM stdin;
\.


--
-- Name: project_last_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_last_update_id_seq', 1, false);


--
-- Data for Name: project_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_tag (id, project, tag) FROM stdin;
\.


--
-- Name: project_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_tag_id_seq', 1, false);


--
-- Data for Name: project_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_update (id, updated_ts, project, author, description) FROM stdin;
\.


--
-- Name: project_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_update_id_seq', 1, false);


--
-- Data for Name: project_user_role; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY project_user_role (id, project, "user", role) FROM stdin;
2	1	1	Admin
3	1	1	Moderator
\.


--
-- Name: project_user_role_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('project_user_role_id_seq', 3, true);


--
-- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY role_event (id, "time", "user", role, project, added) FROM stdin;
1	2014-01-21 10:12:24.376209	1	Moderator	1	t
\.


--
-- Name: role_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('role_event_id_seq', 1, true);


--
-- Data for Name: tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY tag (id, name) FROM stdin;
\.


--
-- Data for Name: tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY tag_color (id, tag, "user", color) FROM stdin;
\.


--
-- Name: tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('tag_color_id_seq', 1, false);


--
-- Name: tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('tag_id_seq', 1, false);


--
-- Data for Name: ticket; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY ticket (id, created_ts, name, comment, updated_ts) FROM stdin;
1	2014-01-21 18:15:30.945499	this is a ticket	4	2014-01-21 18:15:30.945499
\.


--
-- Name: ticket_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('ticket_id_seq', 1, true);


--
-- Data for Name: transaction; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY transaction (id, ts, credit, debit, amount, reason, info, payday) FROM stdin;
\.


--
-- Name: transaction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('transaction_id_seq', 1, false);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY "user" (id, ident, hash, salt, name, account, avatar, blurb, statement, irc_nick, read_messages, read_applications, read_comments, read_edits, established_ts, established_reason) FROM stdin;
1	admin	8bf2d491387febc07e5d8fd15a4140b28473566e	P^YTN3G:	Admin	1	\N	\N	\N	\N	2013-11-23 19:31:18.982213	2013-11-23 19:31:18.982213	2013-11-23 19:31:18.982213	2013-11-23 19:31:18.982213	\N	\N
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_id_seq', 1, true);


--
-- Data for Name: user_setting; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY user_setting (id, "user", setting, value) FROM stdin;
\.


--
-- Name: user_setting_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('user_setting_id_seq', 1, false);


--
-- Data for Name: volunteer_application; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY volunteer_application (id, created_ts, project, "user", name, email, other_contact_info, website, location, experience, comments) FROM stdin;
\.


--
-- Name: volunteer_application_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('volunteer_application_id_seq', 1, false);


--
-- Data for Name: volunteer_interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY volunteer_interest (id, volunteer, interest) FROM stdin;
\.


--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('volunteer_interest_id_seq', 1, false);


--
-- Data for Name: wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_edit (id, ts, "user", page, content, comment) FROM stdin;
1	2014-01-21 17:46:06.695166	1	1	# Welcome\n\nThank you for testing (and hopefully helping to develop) Snowdrift.coop!\n\nThis is a wiki page within your test database. It is different than the database for the real Snowdrift.coop site.	Page created.
2	2014-01-21 17:48:55.489319	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.	Page created.
3	2014-01-21 17:52:33.270443	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.	Added links to wiki page on live site and comment about history
4	2014-01-21 17:53:21.094299	1	2	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.\n\nThere are discussion pages for every wiki page, as shown above.	Added sentence about discussion pages
5	2014-01-21 17:55:07.436846	1	3	See the live site for [press info](https://snowdrift.coop/p/snowdrift/w/press)	Page created.
6	2014-01-21 18:09:53.469506	1	4	# Development notes\n\nSee the live site for the full [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help).\n\n## Development notes\n\nThe essential development details are in the README.md file with the code, not in this test database. When adding new info, consider whether it is best there versus here in the test database (the README has instructions about updating the test database).\n\n## Users\n\n[localhost:3000/u](/u) is a listing of all the users. The first user is just "admin" (passphrase is also "admin"). When new users register they start out unestablished and with no roles. You can add roles by using the admin user and visiting <http://localhost:3000/p/snowdrift/invite> and then logging in as another user to redeem the code.\n\nIt is a good idea to test things as:\n\na. logged-out\na. unestablished user\na. established users with different roles\n\nObviously testing on different systems, browsers, devices, etc. is good too.\n\n## Tickets\n\nSee <https://snowdrift.coop/p/snowdrift/t> for the live site's list of tickets. This is also linked at the live site's how-to-help page. Please add tickets to the live site as appropriate, add comments and questions, and mark things complete after you have fixed them and committed your changes.	Page created.
\.


--
-- Name: wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_edit_id_seq', 6, true);


--
-- Data for Name: wiki_last_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_last_edit (id, page, edit) FROM stdin;
1	1	1
2	2	4
3	3	5
4	4	6
\.


--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_last_edit_id_seq', 4, true);


--
-- Data for Name: wiki_page; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_page (id, target, project, content, permission_level) FROM stdin;
1	intro	1	# Welcome\n\nThank you for testing (and hopefully helping to develop) Snowdrift.coop!\n\nThis is a wiki page within your test database. It is different than the database for the real Snowdrift.coop site.	Normal
2	about	1	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.\n\nThere are discussion pages for every wiki page, as shown above.	Normal
3	press	1	See the live site for [press info](https://snowdrift.coop/p/snowdrift/w/press)	Normal
4	how-to-help	1	# Development notes\n\nSee the live site for the full [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help).\n\n## Development notes\n\nThe essential development details are in the README.md file with the code, not in this test database. When adding new info, consider whether it is best there versus here in the test database (the README has instructions about updating the test database).\n\n## Users\n\n[localhost:3000/u](/u) is a listing of all the users. The first user is just "admin" (passphrase is also "admin"). When new users register they start out unestablished and with no roles. You can add roles by using the admin user and visiting <http://localhost:3000/p/snowdrift/invite> and then logging in as another user to redeem the code.\n\nIt is a good idea to test things as:\n\na. logged-out\na. unestablished user\na. established users with different roles\n\nObviously testing on different systems, browsers, devices, etc. is good too.\n\n## Tickets\n\nSee <https://snowdrift.coop/p/snowdrift/t> for the live site's list of tickets. This is also linked at the live site's how-to-help page. Please add tickets to the live site as appropriate, add comments and questions, and mark things complete after you have fixed them and committed your changes.	Normal
\.


--
-- Data for Name: wiki_page_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_development
--

COPY wiki_page_comment (id, comment, page) FROM stdin;
1	1	2
2	2	2
3	3	2
4	4	2
\.


--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_page_comment_id_seq', 4, true);


--
-- Name: wiki_page_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_development
--

SELECT pg_catalog.setval('wiki_page_id_seq', 4, true);


--
-- Name: account_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: build_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY build
    ADD CONSTRAINT build_pkey PRIMARY KEY (id);


--
-- Name: comment_ancestor_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_pkey PRIMARY KEY (id);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: comment_retraction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);


--
-- Name: comment_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_pkey PRIMARY KEY (id);


--
-- Name: committee_user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_pkey PRIMARY KEY (id);


--
-- Name: database_version_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY database_version
    ADD CONSTRAINT database_version_pkey PRIMARY KEY (id);


--
-- Name: default_tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_pkey PRIMARY KEY (id);


--
-- Name: doc_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_pkey PRIMARY KEY (id);


--
-- Name: doc_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_pkey PRIMARY KEY (id);


--
-- Name: interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY interest
    ADD CONSTRAINT interest_pkey PRIMARY KEY (id);


--
-- Name: invite_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_pkey PRIMARY KEY (id);


--
-- Name: manual_establishment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_pkey PRIMARY KEY (id);


--
-- Name: message_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);


--
-- Name: payday_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY payday
    ADD CONSTRAINT payday_pkey PRIMARY KEY (id);


--
-- Name: pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_pkey PRIMARY KEY (id);


--
-- Name: project_blog_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_pkey PRIMARY KEY (id);


--
-- Name: project_blog_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_pkey PRIMARY KEY (id);


--
-- Name: project_last_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_pkey PRIMARY KEY (id);


--
-- Name: project_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_pkey PRIMARY KEY (id);


--
-- Name: project_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_pkey PRIMARY KEY (id);


--
-- Name: project_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_pkey PRIMARY KEY (id);


--
-- Name: project_user_role_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_pkey PRIMARY KEY (id);


--
-- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_pkey PRIMARY KEY (id);


--
-- Name: tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_pkey PRIMARY KEY (id);


--
-- Name: tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_pkey PRIMARY KEY (id);


--
-- Name: ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_pkey PRIMARY KEY (id);


--
-- Name: transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);


--
-- Name: unique_comment_ancestor; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT unique_comment_ancestor UNIQUE (comment, ancestor);


--
-- Name: unique_comment_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT unique_comment_tag UNIQUE (comment, tag, "user");


--
-- Name: unique_committee_member; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT unique_committee_member UNIQUE ("user");


--
-- Name: unique_default_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT unique_default_tag UNIQUE (tag);


--
-- Name: unique_doc_name; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT unique_doc_name UNIQUE (name);


--
-- Name: unique_invite; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT unique_invite UNIQUE (code);


--
-- Name: unique_manual_establishment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT unique_manual_establishment UNIQUE (established_user);


--
-- Name: unique_pledge; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT unique_pledge UNIQUE ("user", project);


--
-- Name: unique_project_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_account UNIQUE (account);


--
-- Name: unique_project_blog_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT unique_project_blog_comment UNIQUE (comment);


--
-- Name: unique_project_handle; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_handle UNIQUE (handle);


--
-- Name: unique_project_last_update; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT unique_project_last_update UNIQUE (project);


--
-- Name: unique_project_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT unique_project_tag UNIQUE (project, tag);


--
-- Name: unique_project_user_role; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT unique_project_user_role UNIQUE (project, "user", role);


--
-- Name: unique_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT unique_tag UNIQUE (name);


--
-- Name: unique_tag_color; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT unique_tag_color UNIQUE (tag, "user");


--
-- Name: unique_user; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user UNIQUE (ident);


--
-- Name: unique_user_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user_account UNIQUE (account);


--
-- Name: unique_wiki_last_edit; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT unique_wiki_last_edit UNIQUE (page);


--
-- Name: unique_wiki_page_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT unique_wiki_page_comment UNIQUE (comment);


--
-- Name: unique_wiki_target; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT unique_wiki_target UNIQUE (project, target);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: user_setting_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_pkey PRIMARY KEY (id);


--
-- Name: volunteer_application_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_pkey PRIMARY KEY (id);


--
-- Name: volunteer_interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_pkey PRIMARY KEY (id);


--
-- Name: wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_last_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_development; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_pkey PRIMARY KEY (id);


--
-- Name: doc_event; Type: TRIGGER; Schema: public; Owner: snowdrift_development
--

CREATE TRIGGER doc_event AFTER INSERT OR DELETE ON doc FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();


--
-- Name: role_event; Type: TRIGGER; Schema: public; Owner: snowdrift_development
--

CREATE TRIGGER role_event AFTER INSERT OR DELETE ON project_user_role FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();


--
-- Name: comment_ancestor_ancestor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_ancestor_fkey FOREIGN KEY (ancestor) REFERENCES comment(id);


--
-- Name: comment_ancestor_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_by_fkey FOREIGN KEY (moderated_by) REFERENCES "user"(id);


--
-- Name: comment_moderated_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_user_fkey FOREIGN KEY (moderated_by) REFERENCES "user"(id);


--
-- Name: comment_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_parent_fkey FOREIGN KEY (parent) REFERENCES comment(id);


--
-- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: comment_tag_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: comment_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: committee_user_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: committee_user_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: default_tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: doc_current_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_current_version_fkey FOREIGN KEY (current_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_blessed_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_blessed_version_fkey FOREIGN KEY (blessed_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_doc_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_doc_fkey FOREIGN KEY (doc) REFERENCES doc(id);


--
-- Name: invite_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: invite_redeemed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_redeemed_by_fkey FOREIGN KEY (redeemed_by) REFERENCES "user"(id);


--
-- Name: invite_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: message_from_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_from_fkey FOREIGN KEY ("from") REFERENCES "user"(id);


--
-- Name: message_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: message_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: pledge_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: pledge_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: project_blog_comment_blog_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_blog_fkey FOREIGN KEY (blog) REFERENCES project_blog(id);


--
-- Name: project_blog_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: project_blog_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_last_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_last_payday_fkey FOREIGN KEY (last_payday) REFERENCES payday(id);


--
-- Name: project_last_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_last_update_update_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_update_fkey FOREIGN KEY (update) REFERENCES project_update(id);


--
-- Name: project_tag_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: project_update_author_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_author_fkey FOREIGN KEY (author) REFERENCES "user"(id);


--
-- Name: project_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: role_event_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: tag_color_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: ticket_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: transaction_credit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_credit_fkey FOREIGN KEY (credit) REFERENCES account(id);


--
-- Name: transaction_debit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_debit_fkey FOREIGN KEY (debit) REFERENCES account(id);


--
-- Name: transaction_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_payday_fkey FOREIGN KEY (payday) REFERENCES payday(id);


--
-- Name: user_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: user_setting_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_application_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: volunteer_application_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_interest_interest_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_interest_fkey FOREIGN KEY (interest) REFERENCES interest(id);


--
-- Name: volunteer_interest_volunteer_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_volunteer_fkey FOREIGN KEY (volunteer) REFERENCES volunteer_application(id);


--
-- Name: wiki_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_edit_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: wiki_last_edit_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: wiki_last_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: wiki_page_comment_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_development
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

