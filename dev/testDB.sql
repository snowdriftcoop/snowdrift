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
-- Name: log_doc_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_test
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


ALTER FUNCTION public.log_doc_event_trigger() OWNER TO snowdrift_test;

--
-- Name: log_role_event_trigger(); Type: FUNCTION; Schema: public; Owner: snowdrift_test
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


ALTER FUNCTION public.log_role_event_trigger() OWNER TO snowdrift_test;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: account; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE account (
    id integer NOT NULL,
    balance bigint NOT NULL
);


ALTER TABLE public.account OWNER TO snowdrift_test;

--
-- Name: account_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE account_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.account_id_seq OWNER TO snowdrift_test;

--
-- Name: account_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE account_id_seq OWNED BY account.id;


--
-- Name: build; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE build (
    id integer NOT NULL,
    boot_time timestamp without time zone NOT NULL,
    base character varying NOT NULL,
    diff character varying NOT NULL
);


ALTER TABLE public.build OWNER TO snowdrift_test;

--
-- Name: build_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE build_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.build_id_seq OWNER TO snowdrift_test;

--
-- Name: build_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE build_id_seq OWNED BY build.id;


--
-- Name: comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    moderated_ts timestamp without time zone,
    moderated_by bigint,
    parent bigint,
    "user" bigint NOT NULL,
    text character varying NOT NULL,
    depth bigint NOT NULL,
    discussion bigint NOT NULL
);


ALTER TABLE public.comment OWNER TO snowdrift_test;

--
-- Name: comment_ancestor; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_ancestor (
    id integer NOT NULL,
    comment bigint NOT NULL,
    ancestor bigint NOT NULL
);


ALTER TABLE public.comment_ancestor OWNER TO snowdrift_test;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_ancestor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_ancestor_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_ancestor_id_seq OWNED BY comment_ancestor.id;


--
-- Name: comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_id_seq OWNED BY comment.id;


--
-- Name: comment_rethread; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_rethread (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    moderator bigint NOT NULL,
    old_parent bigint,
    old_discussion bigint NOT NULL,
    new_parent bigint,
    new_discussion bigint NOT NULL,
    comment bigint NOT NULL,
    reason character varying NOT NULL
);


ALTER TABLE public.comment_rethread OWNER TO snowdrift_test;

--
-- Name: comment_rethread_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_rethread_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_rethread_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_rethread_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_rethread_id_seq OWNED BY comment_rethread.id;


--
-- Name: comment_retraction; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_retraction (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    reason character varying NOT NULL,
    comment bigint NOT NULL
);


ALTER TABLE public.comment_retraction OWNER TO snowdrift_test;

--
-- Name: comment_retraction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_retraction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_retraction_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_retraction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_retraction_id_seq OWNED BY comment_retraction.id;


--
-- Name: comment_tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE comment_tag (
    id integer NOT NULL,
    comment bigint NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    count bigint DEFAULT 1 NOT NULL
);


ALTER TABLE public.comment_tag OWNER TO snowdrift_test;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE comment_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.comment_tag_id_seq OWNER TO snowdrift_test;

--
-- Name: comment_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE comment_tag_id_seq OWNED BY comment_tag.id;


--
-- Name: committee_user; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE committee_user (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL
);


ALTER TABLE public.committee_user OWNER TO snowdrift_test;

--
-- Name: committee_user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE committee_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.committee_user_id_seq OWNER TO snowdrift_test;

--
-- Name: committee_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE committee_user_id_seq OWNED BY committee_user.id;


--
-- Name: database_version; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE database_version (
    id integer NOT NULL,
    last_migration bigint NOT NULL
);


ALTER TABLE public.database_version OWNER TO snowdrift_test;

--
-- Name: database_version_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE database_version_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.database_version_id_seq OWNER TO snowdrift_test;

--
-- Name: database_version_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE database_version_id_seq OWNED BY database_version.id;


--
-- Name: default_tag_color; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE default_tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.default_tag_color OWNER TO snowdrift_test;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE default_tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.default_tag_color_id_seq OWNER TO snowdrift_test;

--
-- Name: default_tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE default_tag_color_id_seq OWNED BY default_tag_color.id;


--
-- Name: discussion; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE discussion (
    id integer NOT NULL,
    nothing bigint NOT NULL
);


ALTER TABLE public.discussion OWNER TO snowdrift_test;

--
-- Name: discussion_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE discussion_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.discussion_id_seq OWNER TO snowdrift_test;

--
-- Name: discussion_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE discussion_id_seq OWNED BY discussion.id;


--
-- Name: doc; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE doc (
    id integer NOT NULL,
    name character varying NOT NULL,
    current_version bigint NOT NULL
);


ALTER TABLE public.doc OWNER TO snowdrift_test;

--
-- Name: doc_event; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE doc_event (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    doc bigint NOT NULL,
    blessed_version bigint NOT NULL
);


ALTER TABLE public.doc_event OWNER TO snowdrift_test;

--
-- Name: doc_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE doc_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_event_id_seq OWNER TO snowdrift_test;

--
-- Name: doc_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE doc_event_id_seq OWNED BY doc_event.id;


--
-- Name: doc_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE doc_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.doc_id_seq OWNER TO snowdrift_test;

--
-- Name: doc_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE doc_id_seq OWNED BY doc.id;


--
-- Name: interest; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE interest (
    id integer NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.interest OWNER TO snowdrift_test;

--
-- Name: interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.interest_id_seq OWNER TO snowdrift_test;

--
-- Name: interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE interest_id_seq OWNED BY interest.id;


--
-- Name: invite; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
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


ALTER TABLE public.invite OWNER TO snowdrift_test;

--
-- Name: invite_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE invite_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.invite_id_seq OWNER TO snowdrift_test;

--
-- Name: invite_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE invite_id_seq OWNED BY invite.id;


--
-- Name: manual_establishment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE manual_establishment (
    id integer NOT NULL,
    established_user character varying NOT NULL,
    establishing_user character varying NOT NULL
);


ALTER TABLE public.manual_establishment OWNER TO snowdrift_test;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE manual_establishment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.manual_establishment_id_seq OWNER TO snowdrift_test;

--
-- Name: manual_establishment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE manual_establishment_id_seq OWNED BY manual_establishment.id;


--
-- Name: message; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE message (
    id integer NOT NULL,
    project bigint,
    created_ts timestamp without time zone NOT NULL,
    "from" bigint,
    "to" bigint,
    content character varying NOT NULL
);


ALTER TABLE public.message OWNER TO snowdrift_test;

--
-- Name: message_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE message_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.message_id_seq OWNER TO snowdrift_test;

--
-- Name: message_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE message_id_seq OWNED BY message.id;


--
-- Name: payday; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE payday (
    id integer NOT NULL,
    date timestamp without time zone NOT NULL
);


ALTER TABLE public.payday OWNER TO snowdrift_test;

--
-- Name: payday_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE payday_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.payday_id_seq OWNER TO snowdrift_test;

--
-- Name: payday_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE payday_id_seq OWNED BY payday.id;


--
-- Name: pledge; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE pledge (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    shares bigint NOT NULL,
    funded_shares bigint NOT NULL
);


ALTER TABLE public.pledge OWNER TO snowdrift_test;

--
-- Name: pledge_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE pledge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.pledge_id_seq OWNER TO snowdrift_test;

--
-- Name: pledge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE pledge_id_seq OWNED BY pledge.id;


--
-- Name: project; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
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


ALTER TABLE public.project OWNER TO snowdrift_test;

--
-- Name: project_blog; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_blog (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    title character varying NOT NULL,
    "user" bigint NOT NULL,
    top_content character varying NOT NULL,
    project bigint NOT NULL,
    bottom_content character varying,
    discussion bigint NOT NULL
);


ALTER TABLE public.project_blog OWNER TO snowdrift_test;

--
-- Name: project_blog_comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_blog_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    blog bigint NOT NULL
);


ALTER TABLE public.project_blog_comment OWNER TO snowdrift_test;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_blog_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_comment_id_seq OWNER TO snowdrift_test;

--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_blog_comment_id_seq OWNED BY project_blog_comment.id;


--
-- Name: project_blog_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_blog_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_blog_id_seq OWNER TO snowdrift_test;

--
-- Name: project_blog_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_blog_id_seq OWNED BY project_blog.id;


--
-- Name: project_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_id_seq OWNER TO snowdrift_test;

--
-- Name: project_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_id_seq OWNED BY project.id;


--
-- Name: project_last_update; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_last_update (
    id integer NOT NULL,
    project bigint NOT NULL,
    update bigint NOT NULL
);


ALTER TABLE public.project_last_update OWNER TO snowdrift_test;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_last_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_last_update_id_seq OWNER TO snowdrift_test;

--
-- Name: project_last_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_last_update_id_seq OWNED BY project_last_update.id;


--
-- Name: project_tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_tag (
    id integer NOT NULL,
    project bigint NOT NULL,
    tag bigint NOT NULL
);


ALTER TABLE public.project_tag OWNER TO snowdrift_test;

--
-- Name: project_tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_tag_id_seq OWNER TO snowdrift_test;

--
-- Name: project_tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_tag_id_seq OWNED BY project_tag.id;


--
-- Name: project_update; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_update (
    id integer NOT NULL,
    updated_ts timestamp without time zone NOT NULL,
    project bigint NOT NULL,
    author bigint NOT NULL,
    description character varying NOT NULL
);


ALTER TABLE public.project_update OWNER TO snowdrift_test;

--
-- Name: project_update_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_update_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_update_id_seq OWNER TO snowdrift_test;

--
-- Name: project_update_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_update_id_seq OWNED BY project_update.id;


--
-- Name: project_user_role; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE project_user_role (
    id integer NOT NULL,
    project bigint NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL
);


ALTER TABLE public.project_user_role OWNER TO snowdrift_test;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE project_user_role_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.project_user_role_id_seq OWNER TO snowdrift_test;

--
-- Name: project_user_role_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE project_user_role_id_seq OWNED BY project_user_role.id;


--
-- Name: role_event; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE role_event (
    id integer NOT NULL,
    "time" timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    role character varying NOT NULL,
    project bigint NOT NULL,
    added boolean NOT NULL
);


ALTER TABLE public.role_event OWNER TO snowdrift_test;

--
-- Name: role_event_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE role_event_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.role_event_id_seq OWNER TO snowdrift_test;

--
-- Name: role_event_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE role_event_id_seq OWNED BY role_event.id;


--
-- Name: tag; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE tag (
    id integer NOT NULL,
    name character varying NOT NULL
);


ALTER TABLE public.tag OWNER TO snowdrift_test;

--
-- Name: tag_color; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE tag_color (
    id integer NOT NULL,
    tag bigint NOT NULL,
    "user" bigint NOT NULL,
    color bigint NOT NULL
);


ALTER TABLE public.tag_color OWNER TO snowdrift_test;

--
-- Name: tag_color_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE tag_color_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_color_id_seq OWNER TO snowdrift_test;

--
-- Name: tag_color_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE tag_color_id_seq OWNED BY tag_color.id;


--
-- Name: tag_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE tag_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.tag_id_seq OWNER TO snowdrift_test;

--
-- Name: tag_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE tag_id_seq OWNED BY tag.id;


--
-- Name: ticket; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE ticket (
    id integer NOT NULL,
    created_ts timestamp without time zone NOT NULL,
    name character varying NOT NULL,
    comment bigint NOT NULL,
    updated_ts timestamp without time zone NOT NULL
);


ALTER TABLE public.ticket OWNER TO snowdrift_test;

--
-- Name: ticket_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE ticket_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.ticket_id_seq OWNER TO snowdrift_test;

--
-- Name: ticket_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE ticket_id_seq OWNED BY ticket.id;


--
-- Name: transaction; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
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


ALTER TABLE public.transaction OWNER TO snowdrift_test;

--
-- Name: transaction_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE transaction_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.transaction_id_seq OWNER TO snowdrift_test;

--
-- Name: transaction_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE transaction_id_seq OWNED BY transaction.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
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
    established_reason character varying,
    created_ts timestamp without time zone
);


ALTER TABLE public."user" OWNER TO snowdrift_test;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_id_seq OWNER TO snowdrift_test;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: user_setting; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE user_setting (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    setting character varying NOT NULL,
    value character varying NOT NULL
);


ALTER TABLE public.user_setting OWNER TO snowdrift_test;

--
-- Name: user_setting_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE user_setting_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_setting_id_seq OWNER TO snowdrift_test;

--
-- Name: user_setting_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE user_setting_id_seq OWNED BY user_setting.id;


--
-- Name: view_time; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE view_time (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    project bigint NOT NULL,
    type character varying NOT NULL,
    "time" timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.view_time OWNER TO snowdrift_test;

--
-- Name: view_time_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE view_time_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.view_time_id_seq OWNER TO snowdrift_test;

--
-- Name: view_time_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE view_time_id_seq OWNED BY view_time.id;


--
-- Name: volunteer_application; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
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


ALTER TABLE public.volunteer_application OWNER TO snowdrift_test;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE volunteer_application_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_application_id_seq OWNER TO snowdrift_test;

--
-- Name: volunteer_application_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE volunteer_application_id_seq OWNED BY volunteer_application.id;


--
-- Name: volunteer_interest; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE volunteer_interest (
    id integer NOT NULL,
    volunteer bigint NOT NULL,
    interest bigint NOT NULL
);


ALTER TABLE public.volunteer_interest OWNER TO snowdrift_test;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE volunteer_interest_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.volunteer_interest_id_seq OWNER TO snowdrift_test;

--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE volunteer_interest_id_seq OWNED BY volunteer_interest.id;


--
-- Name: wiki_edit; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_edit (
    id integer NOT NULL,
    ts timestamp without time zone NOT NULL,
    "user" bigint NOT NULL,
    page bigint NOT NULL,
    content character varying NOT NULL,
    comment character varying
);


ALTER TABLE public.wiki_edit OWNER TO snowdrift_test;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_edit_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_edit_id_seq OWNED BY wiki_edit.id;


--
-- Name: wiki_last_edit; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_last_edit (
    id integer NOT NULL,
    page bigint NOT NULL,
    edit bigint NOT NULL
);


ALTER TABLE public.wiki_last_edit OWNER TO snowdrift_test;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_last_edit_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_last_edit_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_last_edit_id_seq OWNED BY wiki_last_edit.id;


--
-- Name: wiki_page; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_page (
    id integer NOT NULL,
    target character varying NOT NULL,
    project bigint NOT NULL,
    content character varying NOT NULL,
    permission_level character varying NOT NULL,
    discussion bigint NOT NULL
);


ALTER TABLE public.wiki_page OWNER TO snowdrift_test;

--
-- Name: wiki_page_comment; Type: TABLE; Schema: public; Owner: snowdrift_test; Tablespace: 
--

CREATE TABLE wiki_page_comment (
    id integer NOT NULL,
    comment bigint NOT NULL,
    page bigint NOT NULL
);


ALTER TABLE public.wiki_page_comment OWNER TO snowdrift_test;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_page_comment_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_comment_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_page_comment_id_seq OWNED BY wiki_page_comment.id;


--
-- Name: wiki_page_id_seq; Type: SEQUENCE; Schema: public; Owner: snowdrift_test
--

CREATE SEQUENCE wiki_page_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.wiki_page_id_seq OWNER TO snowdrift_test;

--
-- Name: wiki_page_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: snowdrift_test
--

ALTER SEQUENCE wiki_page_id_seq OWNED BY wiki_page.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY account ALTER COLUMN id SET DEFAULT nextval('account_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY build ALTER COLUMN id SET DEFAULT nextval('build_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment ALTER COLUMN id SET DEFAULT nextval('comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_ancestor ALTER COLUMN id SET DEFAULT nextval('comment_ancestor_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread ALTER COLUMN id SET DEFAULT nextval('comment_rethread_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_retraction ALTER COLUMN id SET DEFAULT nextval('comment_retraction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag ALTER COLUMN id SET DEFAULT nextval('comment_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY committee_user ALTER COLUMN id SET DEFAULT nextval('committee_user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY database_version ALTER COLUMN id SET DEFAULT nextval('database_version_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY default_tag_color ALTER COLUMN id SET DEFAULT nextval('default_tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY discussion ALTER COLUMN id SET DEFAULT nextval('discussion_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc ALTER COLUMN id SET DEFAULT nextval('doc_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc_event ALTER COLUMN id SET DEFAULT nextval('doc_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY interest ALTER COLUMN id SET DEFAULT nextval('interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite ALTER COLUMN id SET DEFAULT nextval('invite_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY manual_establishment ALTER COLUMN id SET DEFAULT nextval('manual_establishment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY message ALTER COLUMN id SET DEFAULT nextval('message_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY payday ALTER COLUMN id SET DEFAULT nextval('payday_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge ALTER COLUMN id SET DEFAULT nextval('pledge_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project ALTER COLUMN id SET DEFAULT nextval('project_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog ALTER COLUMN id SET DEFAULT nextval('project_blog_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog_comment ALTER COLUMN id SET DEFAULT nextval('project_blog_comment_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_last_update ALTER COLUMN id SET DEFAULT nextval('project_last_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_tag ALTER COLUMN id SET DEFAULT nextval('project_tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_update ALTER COLUMN id SET DEFAULT nextval('project_update_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_user_role ALTER COLUMN id SET DEFAULT nextval('project_user_role_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY role_event ALTER COLUMN id SET DEFAULT nextval('role_event_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag ALTER COLUMN id SET DEFAULT nextval('tag_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag_color ALTER COLUMN id SET DEFAULT nextval('tag_color_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket ALTER COLUMN id SET DEFAULT nextval('ticket_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction ALTER COLUMN id SET DEFAULT nextval('transaction_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_setting ALTER COLUMN id SET DEFAULT nextval('user_setting_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_time ALTER COLUMN id SET DEFAULT nextval('view_time_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_application ALTER COLUMN id SET DEFAULT nextval('volunteer_application_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_interest ALTER COLUMN id SET DEFAULT nextval('volunteer_interest_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_edit ALTER COLUMN id SET DEFAULT nextval('wiki_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_last_edit ALTER COLUMN id SET DEFAULT nextval('wiki_last_edit_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page ALTER COLUMN id SET DEFAULT nextval('wiki_page_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page_comment ALTER COLUMN id SET DEFAULT nextval('wiki_page_comment_id_seq'::regclass);


--
-- Data for Name: account; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY account (id, balance) FROM stdin;
1	0
2	0
3	0
\.


--
-- Name: account_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('account_id_seq', 3, true);


--
-- Data for Name: build; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY build (id, boot_time, base, diff) FROM stdin;
1	2013-11-20 02:25:05.013214	8604247d8116396ec0214ba7677d2212990517d2	
2	2013-11-23 19:29:20.349772	8604247d8116396ec0214ba7677d2212990517d2	
3	2014-01-20 19:09:52.001477	4ccd10f42fd6d64ac4e2b3d70b28c9c6a3223b0c	diff --git a/Application.hs b/Application.hs\nindex 241f582..bf838a7 100644\n--- a/Application.hs\n+++ b/Application.hs\n@@ -135,8 +135,8 @@ doMigration = do\n \n     mapM_ ((\\ file -> liftIO (putStrLn ("running " ++ file ++ "...") >> T.readFile file)) >=> flip rawExecute []) $ L.map (("migrations/" <>) . snd) migration_files\n \n-    let last_migration = L.maximum $ 0 : L.map fst migration_files\n-    update $ flip set [ DatabaseVersionLastMigration =. val last_migration ]\n+    let new_last_migration = L.maximum $ 0 : L.map fst migration_files\n+    update $ flip set [ DatabaseVersionLastMigration =. val new_last_migration ]\n \n     migrations <- parseMigration' migrateAll\n \n@@ -145,8 +145,10 @@ doMigration = do\n     liftIO $ putStrLn $ "safe: " ++ show (L.length safe)\n     liftIO $ putStrLn $ "unsafe: " ++ show (L.length unsafe)\n \n+    liftIO $ putStrLn $ "new last_migration: " ++ show new_last_migration\n+\n     when (not $ L.null $ L.map snd safe) $ do\n-        liftIO $ T.writeFile ("migrations/migrate" <> show (last_migration + 1)) $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n+        liftIO $ T.writeFile ("migrations/migrate" <> show (new_last_migration + 1)) $ T.unlines $ L.map ((`snoc` ';') . snd) safe\n         mapM_ (flip rawExecute [] . snd) migrations\n \n     when (not $ L.null $ L.map snd unsafe) $ do\n
4	2014-01-21 17:41:12.047799	ed014b5810941e61f82123f97be0c69600d99f0f	
5	2014-01-21 20:01:56.580066	ed014b5810941e61f82123f97be0c69600d99f0f	
6	2014-01-21 22:30:50.979424	d9c16e7c96ebb0c2e66e9f5e6d2c85abc7aa26dd	
7	2014-01-21 22:58:09.854227	d9c16e7c96ebb0c2e66e9f5e6d2c85abc7aa26dd	
8	2014-01-22 00:06:52.037741	f8d42e0d451310462b2840f2777126be8fd0b162	
9	2014-01-22 00:22:27.746213	1eea4b6d7e8a9a5b82e32a122f04d7611c9e29e3	
10	2014-01-24 06:17:39.744006	0bd171dd45776715e763bd42438f32f9494b4fa5	
11	2014-01-24 07:09:02.600052	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
12	2014-01-24 07:10:14.401191	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..c66e6e4 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -35,7 +35,7 @@ getWikiR project_handle target = do\n \n     when (not can_edit) $ permissionDenied "you do not have permission to edit this page"\n \n-    defaultLayout $ renderWiki' project project_handle target can_edit True page\n+    defaultLayout $ renderWiki project_handle target can_edit True page\n \n renderWiki :: Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wiki")\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
13	2014-01-24 07:14:01.500808	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..a0d3d1e 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -43,7 +43,7 @@ renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wik\n renderWiki' :: Project -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n renderWiki' project project_handle target can_edit can_view_meta page = do\n     setTitle . toHtml $ projectName project `mappend` " Wiki - " `mappend` wikiPageTarget page `mappend` " | Snowdrift.coop"\n-    renderWiki project_handle target can_edit can_view_meta page\n+    renderWiki project_handle target can_edit True page\n \n \n getOldWikiPagesR :: Text -> Handler Html\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
39	2014-02-25 04:08:54.740437	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
40	2014-02-25 04:10:16.285067	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
41	2014-02-25 04:11:44.85321	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
14	2014-01-24 07:23:58.983733	0bd171dd45776715e763bd42438f32f9494b4fa5	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..a0d3d1e 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -43,7 +43,7 @@ renderWiki project_handle target can_edit can_view_meta page = $(widgetFile "wik\n renderWiki' :: Project -> Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n renderWiki' project project_handle target can_edit can_view_meta page = do\n     setTitle . toHtml $ projectName project `mappend` " Wiki - " `mappend` wikiPageTarget page `mappend` " | Snowdrift.coop"\n-    renderWiki project_handle target can_edit can_view_meta page\n+    renderWiki project_handle target can_edit True page\n \n \n getOldWikiPagesR :: Text -> Handler Html\ndiff --git a/templates/default-layout.cassius b/templates/default-layout.cassius\nindex adf4925..bb246ee 100644\n--- a/templates/default-layout.cassius\n+++ b/templates/default-layout.cassius\n@@ -19,7 +19,7 @@ img#logo_m\n \n p.navbar-text\n     font-size: 0.8em\n-    margin-top: 4px\n+    margin-top: 2px\n     margin-bottom: 0\n     padding: 0\n \ndiff --git a/templates/navbar.hamlet b/templates/navbar.hamlet\nindex 7cdc078..fe22941 100644\n--- a/templates/navbar.hamlet\n+++ b/templates/navbar.hamlet\n@@ -17,7 +17,7 @@\n             $maybe Entity user_id _ <- maybe_user\n                 $maybe (balance, pledged) <- money_info\n                     <p .navbar-text .text-center>\n-                        Pledged / Funds \n+                        Pledges / Funds \n                         <br>\n                         <a .navbar-link title="Current total monthly pledge" href="@{UserR user_id}">\n                             #{show pledged}\n
15	2014-01-24 07:53:46.525858	e05ef3d875769e1910908e96aed4ec096ecef498	diff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex c2aeff5..45b8ccb 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -33,8 +33,6 @@ getWikiR project_handle target = do\n \n     let can_edit = isJust $ userEstablishedTs =<< entityVal <$> maybe_user\n \n-    when (not can_edit) $ permissionDenied "you do not have permission to edit this page"\n-\n     defaultLayout $ renderWiki' project project_handle target can_edit True page\n \n renderWiki :: Text -> Text -> Bool -> Bool -> WikiPage -> Widget\n
16	2014-01-24 18:52:45.121638	b857adcedd7be11cf2909b5d6cb536fb17d999c9	
17	2014-01-24 21:47:53.941683	b857adcedd7be11cf2909b5d6cb536fb17d999c9	
18	2014-01-24 23:28:23.255958	b857adcedd7be11cf2909b5d6cb536fb17d999c9	
19	2014-02-04 05:22:47.977252	e42ac19d7713acb15779eb289dc57697a265ffe3	
20	2014-02-23 22:47:53.466385	d028340bcf4a20313dcf303cff2b34fff0c510ea	
21	2014-02-23 22:49:40.628973	d028340bcf4a20313dcf303cff2b34fff0c510ea	
22	2014-02-23 23:52:36.718366	d028340bcf4a20313dcf303cff2b34fff0c510ea	
23	2014-02-24 01:28:00.899965	d028340bcf4a20313dcf303cff2b34fff0c510ea	
24	2014-02-24 01:58:36.425711	de72f403beece572fdb23ba0c966bbf77f689415	diff --git a/Handler/Discussion.hs b/Handler/Discussion.hs\nindex 401ffb9..301647a 100644\n--- a/Handler/Discussion.hs\n+++ b/Handler/Discussion.hs\n@@ -26,11 +26,12 @@ import Widgets.Time\n import Yesod.Markdown\n import Model.Markdown\n \n+import Yesod.Default.Config\n \n-renderComment :: UserId -> Text -> Text -> M.Map UserId (Entity User) -> Int -> Int\n+renderComment :: UserId -> [Role] -> Text -> Text -> M.Map UserId (Entity User) -> Int -> Int\n     -> [CommentRetraction] -> M.Map CommentId CommentRetraction -> Bool -> Map TagId Tag -> Tree (Entity Comment) -> Maybe Widget -> Widget\n \n-renderComment viewer_id project_handle target users max_depth depth earlier_retractions retraction_map show_actions tag_map tree mcomment_form = do\n+renderComment viewer_id viewer_roles project_handle target users max_depth depth earlier_retractions retraction_map show_actions tag_map tree mcomment_form = do\n     maybe_route <- handlerToWidget getCurrentRoute\n     (comment_form, _) <- handlerToWidget $ generateFormPost $ commentForm Nothing Nothing\n \n@@ -49,6 +50,9 @@ renderComment viewer_id project_handle target users max_depth depth earlier_retr\n         maybe_retraction = M.lookup comment_id retraction_map\n         empty_list = []\n \n+        user_is_mod = elem Moderator viewer_roles\n+        can_rethread = user_id == viewer_id || user_is_mod\n+\n     tags <- fmap (L.sortBy (compare `on` atName)) $ handlerToWidget $ do\n         comment_tags <- runDB $ select $ from $ \\ comment_tag -> do\n             where_ $ comment_tag ^. CommentTagComment ==. val comment_id\n@@ -152,6 +156,7 @@ getRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n getRetractWikiCommentR project_handle target comment_id = do\n     Entity user_id user <- requireAuth\n     comment <- runDB $ get404 comment_id\n+    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle\n     when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."\n \n     earlier_retractions <- runDB $\n@@ -179,7 +184,9 @@ getRetractWikiCommentR project_handle target comment_id = do\n \n     (retract_form, _) <- generateFormPost $ retractForm Nothing\n \n-    let rendered_comment = renderDiscussComment user_id project_handle target False (return ()) (Entity comment_id comment) [] (M.singleton user_id $ Entity user_id user) earlier_retractions M.empty False tag_map\n+    roles <- getRoles user_id project_id\n+\n+    let rendered_comment = renderDiscussComment user_id roles project_handle target False (return ()) (Entity comment_id comment) [] (M.singleton user_id $ Entity user_id user) earlier_retractions M.empty False tag_map\n \n     defaultLayout $ [whamlet|\n         ^{rendered_comment}\n@@ -196,6 +203,7 @@ postRetractWikiCommentR :: Text -> Text -> CommentId -> Handler Html\n postRetractWikiCommentR project_handle target comment_id = do\n     Entity user_id user <- requireAuth\n     comment <- runDB $ get404 comment_id\n+    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle\n     when (commentUser comment /= user_id) $ permissionDenied "You can only retract your own comments."\n \n     ((result, _), _) <- runFormPost $ retractForm Nothing\n@@ -233,7 +241,9 @@ postRetractWikiCommentR project_handle target comment_id = do\n                         users = M.singleton user_id $ Entity user_id user\n                         retractions = M.singleton comment_id retraction\n \n-                    defaultLayout $ renderPreview form action $ renderDiscussComment user_id project_handle target False (return ()) comment_entity [] users earlier_retractions retractions False tag_map\n+                    roles <- getRoles user_id project_id\n+\n+                    defaultLayout $ renderPreview form action $ renderDiscussComment user_id roles project_handle target False (return ()) comment_entity [] users earlier_retractions retractions False tag_map\n \n \n                 Just a | a == action -> do\n@@ -306,9 +316,11 @@ getDiscussWikiR project_handle target = do\n \n     tags <- runDB $ select $ from $ return\n \n+    roles <- getRoles user_id project_id\n+\n     let tag_map = M.fromList $ entityPairs tags\n         comments = forM_ roots $ \\ root ->\n-            renderComment user_id project_handle target users 10 0 [] retraction_map True tag_map (buildCommentTree root rest) Nothing\n+            renderComment user_id roles project_handle target users 10 0 [] retraction_map True tag_map (buildCommentTree root rest) Nothing\n \n     (comment_form, _) <- generateFormPost $ commentForm Nothing Nothing\n \n@@ -333,8 +345,8 @@ getReplyCommentR =\n getDiscussCommentR' :: Bool -> Text -> Text -> CommentId -> Handler Html\n getDiscussCommentR' show_reply project_handle target comment_id = do\n     Entity viewer_id _ <- requireAuth\n+    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle\n     Entity page_id page  <- runDB $ do\n-        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle\n         getBy404 $ UniqueWikiTarget project_id target\n \n     (root, rest, users, earlier_retractions, retraction_map) <- runDB $ do\n@@ -383,19 +395,21 @@ getDiscussCommentR' show_reply project_handle target comment_id = do\n \n     let tag_map = M.fromList $ entityPairs tags\n \n-    defaultLayout $ renderDiscussComment viewer_id project_handle target show_reply comment_form (Entity comment_id root) rest users earlier_retractions retraction_map True tag_map\n+    roles <- getRoles viewer_id project_id\n+\n+    defaultLayout $ renderDiscussComment viewer_id roles project_handle target show_reply comment_form (Entity comment_id root) rest users earlier_retractions retraction_map True tag_map\n \n \n-renderDiscussComment :: UserId -> Text -> Text -> Bool -> Widget\n+renderDiscussComment :: UserId -> [Role] -> Text -> Text -> Bool -> Widget\n     -> Entity Comment -> [Entity Comment]\n     -> M.Map UserId (Entity User)\n     -> [CommentRetraction]\n     -> M.Map CommentId CommentRetraction\n     -> Bool -> M.Map TagId Tag -> Widget\n \n-renderDiscussComment viewer_id project_handle target show_reply comment_form root rest users earlier_retractions retraction_map show_actions tag_map = do\n+renderDiscussComment viewer_id roles project_handle target show_reply comment_form root rest users earlier_retractions retraction_map show_actions tag_map = do\n     let tree = buildCommentTree root rest\n-        comment = renderComment viewer_id project_handle target users 1 0 earlier_retractions retraction_map show_actions tag_map tree mcomment_form\n+        comment = renderComment viewer_id roles project_handle target users 1 0 earlier_retractions retraction_map show_actions tag_map tree mcomment_form\n         mcomment_form =\n             if show_reply\n                 then Just comment_form\n@@ -410,8 +424,8 @@ postOldDiscussWikiR = postDiscussWikiR\n postDiscussWikiR :: Text -> Text -> Handler Html\n postDiscussWikiR project_handle target = do\n     Entity user_id user <- requireAuth\n+    Entity project_id _ <- runDB $ getBy404 $ UniqueProjectHandle project_handle\n     Entity _ page <- runDB $ do\n-        Entity project_id _ <- getBy404 $ UniqueProjectHandle project_handle\n         getBy404 $ UniqueWikiTarget project_id target\n \n \n@@ -454,9 +468,11 @@ postDiscussWikiR project_handle target = do\n \n                     (form, _) <- generateFormPost $ commentForm maybe_parent_id (Just text)\n \n+                    roles <- getRoles user_id project_id\n+\n                     let comment = Entity (Key $ PersistInt64 0) $ Comment now Nothing Nothing (wikiPageDiscussion page) maybe_parent_id user_id text depth\n                         user_map = M.singleton user_id $ Entity user_id user\n-                        rendered_comment = renderDiscussComment user_id project_handle target False (return ()) comment [] user_map earlier_retractions M.empty False tag_map\n+                        rendered_comment = renderDiscussComment user_id roles project_handle target False (return ()) comment [] user_map earlier_retractions M.empty False tag_map\n \n                     defaultLayout $ renderPreview form action rendered_comment\n \n@@ -580,6 +596,8 @@ getWikiNewCommentsR project_handle = do\n \n         return (comments, pages, users, retraction_map)\n \n+    roles <- getRoles viewer_id project_id\n+\n     let PersistInt64 to = unKey $ minimum (map entityKey comments)\n         rendered_comments =\n             if null comments\n@@ -600,7 +618,7 @@ getWikiNewCommentsR project_handle = do\n                     where_ $ c ^. CommentId ==. val comment_id\n                     return $ p ^. WikiPageTarget\n \n-                let rendered_comment = renderComment viewer_id project_handle target users 1 0 earlier_retractions retraction_map True tag_map (Node (Entity comment_id comment) []) Nothing\n+                let rendered_comment = renderComment viewer_id roles project_handle target users 1 0 earlier_retractions retraction_map True tag_map (Node (Entity comment_id comment) []) Nothing\n \n                 [whamlet|$newline never\n                     <div .row>\n@@ -659,7 +677,11 @@ postRethreadWikiCommentR project_handle target comment_id = do\n \n     case result of\n         FormSuccess (new_parent_url, reason) -> do\n-            let url = T.splitOn "/" $ fst $ T.break (== '?') new_parent_url\n+            app <- getYesod\n+            let splitPath = drop 1 . T.splitOn "/"\n+                stripQuery = fst . T.break (== '?')\n+                stripRoot = maybe new_parent_url id . T.stripPrefix (appRoot $ settings app)\n+                url = splitPath $ stripQuery $ stripRoot new_parent_url\n \n             (new_parent_id, new_discussion_id) <- case parseRoute (url, []) of\n                 Just (DiscussCommentR new_project_handle new_target new_parent_id) -> do\n@@ -710,7 +732,7 @@ postRethreadWikiCommentR project_handle target comment_id = do\n             mode <- lookupPostParam "mode"\n             let action :: Text = "rethread"\n             case mode of\n-                Just "preview" -> error "no preview for rethreads yet"\n+                Just "preview" -> error "no preview for rethreads yet" -- TODO\n \n                 Just a | a == action -> do\n                     now <- liftIO getCurrentTime\n@@ -734,10 +756,25 @@ postRethreadWikiCommentR project_handle target comment_id = do\n \n                         let descendents = comment_id : map (\\ (Value x) -> x) descendents'\n \n-                        when (not $ null old_ancestors) $ delete $ from $ \\ comment_ancestor -> where_ $ comment_ancestor ^. CommentAncestorComment `in_` valList old_ancestors\n-                                                                                                    &&. comment_ancestor ^. CommentAncestorAncestor `in_` valList descendents\n+                        when (not $ null old_ancestors) $ do\n+                            to_delete <- select $ from $ \\ comment_ancestor -> do\n+                                where_ $ comment_ancestor ^. CommentAncestorComment `in_` valList old_ancestors\n+                                        &&. comment_ancestor ^. CommentAncestorAncestor `in_` valList descendents\n+                                return comment_ancestor\n+\n+                            liftIO $ print to_delete\n+\n+                            delete $ from $ \\ comment_ancestor -> do\n+                                where_ $ comment_ancestor ^. CommentAncestorComment `in_` valList old_ancestors\n+                                        &&. comment_ancestor ^. CommentAncestorAncestor `in_` valList descendents\n+\n+                            update $ \\ c -> do\n+                                where_ $ c ^. CommentId ==. val comment_id\n+                                set c [ CommentParent =. val new_parent_id ]\n \n-                        forM_ new_ancestors $ \\ new_ancestor_id -> forM_ descendents $ \\ descendent -> insert_ $ CommentAncestor descendent new_ancestor_id\n+                        forM_ new_ancestors $ \\ new_ancestor_id -> forM_ descendents $ \\ descendent -> do\n+                            liftIO $ putStrLn $ "inserting comment ancestor " ++ show descendent ++ " " ++ show new_ancestor_id\n+                            insert_ $ CommentAncestor descendent new_ancestor_id\n \n                         when (new_discussion_id /= commentDiscussion comment) $ update $ \\ c -> do\n                                 where_ $ c ^. CommentId `in_` valList descendents\ndiff --git a/Handler/Wiki.hs b/Handler/Wiki.hs\nindex 2999bd5..b8c3f76 100644\n--- a/Handler/Wiki.hs\n+++ b/Handler/Wiki.hs\n@@ -282,8 +282,8 @@ postNewWikiR project_handle target = do\n \n                 Just x | x == action -> do\n                     _ <- runDB $ do\n-                        discussion_id <- insert Discussion\n-                        page_id <- insert $ WikiPage target project_id content discussion_id Normal\n+                        discussion <- insert (Discussion 0)\n+                        page_id <- insert $ WikiPage target project_id content discussion Normal\n                         edit_id <- insert $ WikiEdit now user_id page_id content $ Just "Page created."\n                         insert $ WikiLastEdit page_id edit_id\n \ndiff --git a/Model/Role.hs b/Model/Role.hs\nindex 1d744b8..1f6fe53 100644\n--- a/Model/Role.hs\n+++ b/Model/Role.hs\n@@ -4,12 +4,18 @@ module Model.Role\n     , roleLabel\n     , roleAbbrev\n     , roleField\n+    , getRoles\n     ) where\n \n import Import\n \n import Model.Role.Internal\n \n+getRoles :: UserId -> ProjectId -> Handler [Role]\n+getRoles user_id project_id = fmap (map (\\ (Value a) -> a)) $ runDB $ select $ from $ \\ r -> do\n+    where_ $ r ^. ProjectUserRoleProject ==. val project_id\n+            &&. r ^. ProjectUserRoleUser ==. val user_id\n+    return $ r ^. ProjectUserRoleRole\n \n roleLabel :: Role -> Text\n roleLabel TeamMember = "Team Member"\ndiff --git a/config/models b/config/models\nindex bc76ad0..5642ab8 100644\n--- a/config/models\n+++ b/config/models\n@@ -171,6 +171,7 @@ WikiLastEdit\n     UniqueWikiLastEdit page\n \n Discussion\n+    nothing Int64\n \n Comment\n     createdTs UTCTime\n@@ -187,6 +188,7 @@ CommentAncestor\n     comment CommentId\n     ancestor CommentId\n     UniqueCommentAncestor comment ancestor\n+    deriving Show\n \n CommentRetraction\n     ts UTCTime\ndiff --git a/config/routes b/config/routes\nindex c8b736b..132e8b3 100644\n--- a/config/routes\n+++ b/config/routes\n@@ -41,7 +41,7 @@\n /p/#Text/w/#Text/c/#CommentId DiscussCommentR GET\n /p/#Text/w/#Text/c/#CommentId/reply ReplyCommentR GET\n /p/#Text/w/#Text/c/#CommentId/moderate ApproveWikiCommentR GET POST\n-/p/#Text/w/#Text/c/#CommentId/rethread RethreadWikiCommentR POST\n+/p/#Text/w/#Text/c/#CommentId/rethread RethreadWikiCommentR GET POST\n /p/#Text/w/#Text/c/#CommentId/retract RetractWikiCommentR GET POST\n /p/#Text/w/#Text/c/#CommentId/tags CommentTagsR GET\n /p/#Text/w/#Text/c/#CommentId/tag/#TagId CommentTagR GET POST\ndiff --git a/templates/comment_body.hamlet b/templates/comment_body.hamlet\nindex abab7d1..8ea1d23 100644\n--- a/templates/comment_body.hamlet\n+++ b/templates/comment_body.hamlet\n@@ -43,10 +43,11 @@ $# used by renderComment function\n \n     <div>\n         $if show_actions\n-            $if unapproved\n-                <span .comment-action>\n-                    <a href="@{ApproveWikiCommentR project_handle target comment_id}">\n-                        approve\n+            $if user_is_mod\n+                $if unapproved\n+                    <span .comment-action>\n+                        <a href="@{ApproveWikiCommentR project_handle target comment_id}">\n+                            approve\n \n             $if not (maybe_route == Just (ReplyCommentR project_handle target comment_id))\n                 <span .comment-action>\n@@ -58,6 +59,11 @@ $# used by renderComment function\n                     <a href="@{RetractWikiCommentR project_handle target comment_id}" style="color: darkred">\n                         retract\n \n+            $if can_rethread\n+                <span .comment-action>\n+                    <a href="@{RethreadWikiCommentR project_handle target comment_id}">\n+                        rethread\n+\n             ^{newTagWidget $ NewCommentTagR project_handle target comment_id}\n \n     $maybe comment_form <- mcomment_form\n@@ -77,5 +83,5 @@ $# used by renderComment function\n \n     $else\n         $forall child <- children\n-            ^{renderComment viewer_id project_handle target users max_depth (depth + 1) empty_list retraction_map show_actions tag_map child Nothing}\n+            ^{renderComment viewer_id viewer_roles project_handle target users max_depth (depth + 1) empty_list retraction_map show_actions tag_map child Nothing}\n \n
25	2014-02-24 14:12:18.368834	d028340bcf4a20313dcf303cff2b34fff0c510ea	
26	2014-02-25 00:42:20.537308	d028340bcf4a20313dcf303cff2b34fff0c510ea	
27	2014-02-25 00:43:18.475595	d028340bcf4a20313dcf303cff2b34fff0c510ea	
28	2014-02-25 00:49:35.293567	d028340bcf4a20313dcf303cff2b34fff0c510ea	
29	2014-02-25 00:50:04.649503	d028340bcf4a20313dcf303cff2b34fff0c510ea	
30	2014-02-25 00:51:16.747488	d028340bcf4a20313dcf303cff2b34fff0c510ea	
31	2014-02-25 00:55:37.250279	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
32	2014-02-25 00:58:06.088436	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
33	2014-02-25 01:32:27.387693	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
34	2014-02-25 01:33:15.879768	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
35	2014-02-25 01:34:35.980916	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
36	2014-02-25 01:36:58.43768	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
37	2014-02-25 01:39:19.029505	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
38	2014-02-25 01:41:22.819967	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
42	2014-02-25 04:13:39.969239	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
43	2014-02-25 04:14:35.197567	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
44	2014-02-25 04:21:49.989436	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
45	2014-02-25 04:36:15.577906	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
46	2014-02-25 05:11:12.304015	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
47	2014-02-25 05:13:30.156508	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
48	2014-02-25 05:14:37.281799	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
49	2014-02-25 05:17:50.893823	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
50	2014-02-25 05:20:08.027469	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
51	2014-02-25 05:23:02.384789	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
52	2014-02-25 05:24:20.270556	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
53	2014-02-25 05:31:28.295731	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
54	2014-02-25 05:33:58.402282	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
55	2014-02-25 05:37:43.30015	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
56	2014-02-25 05:38:26.680714	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
57	2014-02-25 05:40:47.826576	d028340bcf4a20313dcf303cff2b34fff0c510ea	diff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 0e94dce..16e0bd9 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -222,4 +222,9 @@ test-suite test\n                  , resourcet\n                  , monad-logger\n                  , transformers\n+                 , bytestring\n+                 , text\n                  , hspec\n+                 , network\n+                 , http-types\n+                 , wai-test\n
58	2014-03-02 05:44:50.698901	008e9bc87dbbab3764cfac6ac19bd3db630387d4	diff --git a/Import.hs b/Import.hs\nindex 09eb514..0c29391 100644\n--- a/Import.hs\n+++ b/Import.hs\n@@ -175,3 +175,4 @@ renderBootstrap3 aform fragment = do\n                 |]\n     return (res, widget)\n \n+\ndiff --git a/Settings.hs b/Settings.hs\nindex e303486..ad348e9 100644\n--- a/Settings.hs\n+++ b/Settings.hs\n@@ -57,8 +57,6 @@ widgetFileSettings = def\n         }\n     }\n \n--- The rest of this file contains settings which rarely need changing by a\n--- user.\n \n widgetFile :: String -> Q Exp\n widgetFile = (if development then widgetFileReload\ndiff --git a/Snowdrift.cabal b/Snowdrift.cabal\nindex 16e0bd9..87f98b3 100644\n--- a/Snowdrift.cabal\n+++ b/Snowdrift.cabal\n@@ -228,3 +228,5 @@ test-suite test\n                  , network\n                  , http-types\n                  , wai-test\n+                 , unix\n+                 , mtl\n
\.


--
-- Name: build_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('build_id_seq', 58, true);


--
-- Data for Name: comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment (id, created_ts, moderated_ts, moderated_by, parent, "user", text, depth, discussion) FROM stdin;
1	2014-01-21 18:11:03.914397	2014-01-21 18:12:36.696658	1	\N	1	This is a comment.	0	2
2	2014-01-21 18:13:00.273315	2014-01-21 18:13:10.464805	1	1	1	Replies are threaded.	1	2
3	2014-01-21 18:13:57.732222	\N	\N	\N	1	When a comment is posted by an unestablished user, it is marked for moderation and only shown to moderators.	0	2
4	2014-01-21 18:15:30.945499	2014-01-21 18:15:37.484472	1	\N	1	adding a line starting with "ticket:" such as\n\nticket: this is a ticket\n\nmakes the post show up at /t where all the tickets are listed	0	2
\.


--
-- Data for Name: comment_ancestor; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_ancestor (id, comment, ancestor) FROM stdin;
1	2	1
\.


--
-- Name: comment_ancestor_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_ancestor_id_seq', 1, true);


--
-- Name: comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_id_seq', 4, true);


--
-- Data for Name: comment_rethread; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_rethread (id, ts, moderator, old_parent, old_discussion, new_parent, new_discussion, comment, reason) FROM stdin;
\.


--
-- Name: comment_rethread_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_rethread_id_seq', 1, false);


--
-- Data for Name: comment_retraction; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_retraction (id, ts, reason, comment) FROM stdin;
\.


--
-- Name: comment_retraction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_retraction_id_seq', 1, false);


--
-- Data for Name: comment_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY comment_tag (id, comment, tag, "user", count) FROM stdin;
\.


--
-- Name: comment_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('comment_tag_id_seq', 1, false);


--
-- Data for Name: committee_user; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY committee_user (id, created_ts, "user", project) FROM stdin;
\.


--
-- Name: committee_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('committee_user_id_seq', 1, false);


--
-- Data for Name: database_version; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY database_version (id, last_migration) FROM stdin;
1	5
\.


--
-- Name: database_version_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('database_version_id_seq', 1, true);


--
-- Data for Name: default_tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY default_tag_color (id, tag, color) FROM stdin;
\.


--
-- Name: default_tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('default_tag_color_id_seq', 1, false);


--
-- Data for Name: discussion; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY discussion (id, nothing) FROM stdin;
1	0
2	0
3	0
4	0
\.


--
-- Name: discussion_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('discussion_id_seq', 4, true);


--
-- Data for Name: doc; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY doc (id, name, current_version) FROM stdin;
\.


--
-- Data for Name: doc_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY doc_event (id, "time", doc, blessed_version) FROM stdin;
\.


--
-- Name: doc_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('doc_event_id_seq', 1, false);


--
-- Name: doc_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('doc_id_seq', 1, false);


--
-- Data for Name: interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY interest (id, description) FROM stdin;
\.


--
-- Name: interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('interest_id_seq', 1, false);


--
-- Data for Name: invite; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY invite (id, created_ts, project, code, "user", role, tag, redeemed, redeemed_ts, redeemed_by) FROM stdin;
1	2014-01-21 18:12:09.148007	1	df0176d67f1a4063	1	Moderator	admin as also moderator	t	2014-01-21 18:12:24.376052	1
2	2014-01-24 23:33:43.323505	1	e3ed7c9e1500fc54	1	TeamMember	admin as also team member	t	2014-01-24 23:33:53.481901	1
\.


--
-- Name: invite_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('invite_id_seq', 2, true);


--
-- Data for Name: manual_establishment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY manual_establishment (id, established_user, establishing_user) FROM stdin;
\.


--
-- Name: manual_establishment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('manual_establishment_id_seq', 1, false);


--
-- Data for Name: message; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY message (id, project, created_ts, "from", "to", content) FROM stdin;
1	1	2014-01-21 22:31:51.496246	1	\N	Welcome!
\.


--
-- Name: message_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('message_id_seq', 1, true);


--
-- Data for Name: payday; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY payday (id, date) FROM stdin;
\.


--
-- Name: payday_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('payday_id_seq', 1, false);


--
-- Data for Name: pledge; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY pledge (id, "user", project, shares, funded_shares) FROM stdin;
\.


--
-- Name: pledge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('pledge_id_seq', 1, false);


--
-- Data for Name: project; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project (id, created_ts, name, handle, description, account, share_value, last_payday, github_repo) FROM stdin;
1	2013-11-23 11:52:54.632763	Snowdrift.coop	snowdrift	The Snowdrift.coop site is itself one of the projects.	2	0	\N	\N
\.


--
-- Data for Name: project_blog; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_blog (id, "time", title, "user", top_content, project, bottom_content, discussion) FROM stdin;
\.


--
-- Data for Name: project_blog_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_blog_comment (id, comment, blog) FROM stdin;
\.


--
-- Name: project_blog_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_blog_comment_id_seq', 1, false);


--
-- Name: project_blog_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_blog_id_seq', 1, false);


--
-- Name: project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_id_seq', 1, true);


--
-- Data for Name: project_last_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_last_update (id, project, update) FROM stdin;
1	1	1
\.


--
-- Name: project_last_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_last_update_id_seq', 1, true);


--
-- Data for Name: project_tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_tag (id, project, tag) FROM stdin;
1	1	1
\.


--
-- Name: project_tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_tag_id_seq', 1, true);


--
-- Data for Name: project_update; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_update (id, updated_ts, project, author, description) FROM stdin;
1	2014-01-24 21:49:51.132962	1	1	MarkdownDiff [(F,"Snowdrift Project"),(S,"The Snowdrift.coop site is itself one of the projects.")]
\.


--
-- Name: project_update_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_update_id_seq', 1, true);


--
-- Data for Name: project_user_role; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY project_user_role (id, project, "user", role) FROM stdin;
2	1	1	Admin
3	1	1	Moderator
4	1	1	TeamMember
5	1	2	Moderator
\.


--
-- Name: project_user_role_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('project_user_role_id_seq', 5, true);


--
-- Data for Name: role_event; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY role_event (id, "time", "user", role, project, added) FROM stdin;
1	2014-01-21 10:12:24.376209	1	Moderator	1	t
2	2014-01-24 15:33:53.482076	1	TeamMember	1	t
3	2014-03-07 19:13:15.080277	2	Moderator	1	t
\.


--
-- Name: role_event_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('role_event_id_seq', 3, true);


--
-- Data for Name: tag; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY tag (id, name) FROM stdin;
1	website
\.


--
-- Data for Name: tag_color; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY tag_color (id, tag, "user", color) FROM stdin;
\.


--
-- Name: tag_color_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('tag_color_id_seq', 1, false);


--
-- Name: tag_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('tag_id_seq', 1, true);


--
-- Data for Name: ticket; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY ticket (id, created_ts, name, comment, updated_ts) FROM stdin;
1	2014-01-21 18:15:30.945499	this is a ticket	4	2014-01-21 18:15:30.945499
\.


--
-- Name: ticket_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('ticket_id_seq', 1, true);


--
-- Data for Name: transaction; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY transaction (id, ts, credit, debit, amount, reason, info, payday) FROM stdin;
\.


--
-- Name: transaction_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('transaction_id_seq', 1, false);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY "user" (id, ident, hash, salt, name, account, avatar, blurb, statement, irc_nick, read_messages, read_applications, read_comments, read_edits, established_ts, established_reason, created_ts) FROM stdin;
1	admin	8bf2d491387febc07e5d8fd15a4140b28473566e	P^YTN3G:	Admin	1	\N	Admin is the name for the test user in our devDB database that comes with the code. Log in as admin with passphrase: admin	\N	\N	2014-01-21 22:58:23.380462	2013-11-23 19:31:18.982213	2013-11-23 19:31:18.982213	2013-11-23 19:31:18.982213	2014-01-24 15:28:15.681117	\N	\N
2	test	a090d14299acd2b596b64fb5a46d3587ece359d8	_>4icWF[	Test	3	\N	\N	\N	\N	2014-02-24 01:58:57.856901	2014-02-24 01:58:57.856901	2014-02-24 01:58:57.856901	2014-02-24 01:58:57.856901	\N	\N	2014-02-24 01:58:57.856901
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_id_seq', 2, true);


--
-- Data for Name: user_setting; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY user_setting (id, "user", setting, value) FROM stdin;
\.


--
-- Name: user_setting_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('user_setting_id_seq', 1, false);


--
-- Data for Name: view_time; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY view_time (id, "user", project, type, "time") FROM stdin;
\.


--
-- Name: view_time_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('view_time_id_seq', 1, false);


--
-- Data for Name: volunteer_application; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY volunteer_application (id, created_ts, project, "user", name, email, other_contact_info, website, location, experience, comments) FROM stdin;
\.


--
-- Name: volunteer_application_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('volunteer_application_id_seq', 1, false);


--
-- Data for Name: volunteer_interest; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY volunteer_interest (id, volunteer, interest) FROM stdin;
\.


--
-- Name: volunteer_interest_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('volunteer_interest_id_seq', 1, false);


--
-- Data for Name: wiki_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
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
-- Name: wiki_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_edit_id_seq', 6, true);


--
-- Data for Name: wiki_last_edit; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_last_edit (id, page, edit) FROM stdin;
1	1	1
2	2	4
3	3	5
4	4	6
\.


--
-- Name: wiki_last_edit_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_last_edit_id_seq', 4, true);


--
-- Data for Name: wiki_page; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_page (id, target, project, content, permission_level, discussion) FROM stdin;
1	intro	1	# Welcome\n\nThank you for testing (and hopefully helping to develop) Snowdrift.coop!\n\nThis is a wiki page within your test database. It is different than the database for the real Snowdrift.coop site.	Normal	1
2	about	1	# About Snowdrift.coop\n\nAll the real *about* stuff is on the live site: <https://snowdrift.coop/p/snowdrift/w/about>\n\nHere we will explain about testing.\n\n## Wiki pages\n\nSee the live site for details about the wiki system: <https://snowdrift.coop/p/snowdrift/w/wiki>\n\nIn creating the page you are looking at, several edits were made, so you can click above to see the history.\n\nThere are discussion pages for every wiki page, as shown above.	Normal	2
3	press	1	See the live site for [press info](https://snowdrift.coop/p/snowdrift/w/press)	Normal	3
4	how-to-help	1	# Development notes\n\nSee the live site for the full [how-to-help page](https://snowdrift.coop/p/snowdrift/w/how-to-help).\n\n## Development notes\n\nThe essential development details are in the README.md file with the code, not in this test database. When adding new info, consider whether it is best there versus here in the test database (the README has instructions about updating the test database).\n\n## Users\n\n[localhost:3000/u](/u) is a listing of all the users. The first user is just "admin" (passphrase is also "admin"). When new users register they start out unestablished and with no roles. You can add roles by using the admin user and visiting <http://localhost:3000/p/snowdrift/invite> and then logging in as another user to redeem the code.\n\nIt is a good idea to test things as:\n\na. logged-out\na. unestablished user\na. established users with different roles\n\nObviously testing on different systems, browsers, devices, etc. is good too.\n\n## Tickets\n\nSee <https://snowdrift.coop/p/snowdrift/t> for the live site's list of tickets. This is also linked at the live site's how-to-help page. Please add tickets to the live site as appropriate, add comments and questions, and mark things complete after you have fixed them and committed your changes.	Normal	4
\.


--
-- Data for Name: wiki_page_comment; Type: TABLE DATA; Schema: public; Owner: snowdrift_test
--

COPY wiki_page_comment (id, comment, page) FROM stdin;
1	1	2
2	2	2
3	3	2
4	4	2
\.


--
-- Name: wiki_page_comment_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_page_comment_id_seq', 4, true);


--
-- Name: wiki_page_id_seq; Type: SEQUENCE SET; Schema: public; Owner: snowdrift_test
--

SELECT pg_catalog.setval('wiki_page_id_seq', 4, true);


--
-- Name: account_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY account
    ADD CONSTRAINT account_pkey PRIMARY KEY (id);


--
-- Name: build_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY build
    ADD CONSTRAINT build_pkey PRIMARY KEY (id);


--
-- Name: comment_ancestor_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_pkey PRIMARY KEY (id);


--
-- Name: comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_pkey PRIMARY KEY (id);


--
-- Name: comment_rethread_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_pkey PRIMARY KEY (id);


--
-- Name: comment_retraction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_pkey PRIMARY KEY (id);


--
-- Name: comment_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_pkey PRIMARY KEY (id);


--
-- Name: committee_user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_pkey PRIMARY KEY (id);


--
-- Name: database_version_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY database_version
    ADD CONSTRAINT database_version_pkey PRIMARY KEY (id);


--
-- Name: default_tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_pkey PRIMARY KEY (id);


--
-- Name: discussion_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY discussion
    ADD CONSTRAINT discussion_pkey PRIMARY KEY (id);


--
-- Name: doc_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_pkey PRIMARY KEY (id);


--
-- Name: doc_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_pkey PRIMARY KEY (id);


--
-- Name: interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY interest
    ADD CONSTRAINT interest_pkey PRIMARY KEY (id);


--
-- Name: invite_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_pkey PRIMARY KEY (id);


--
-- Name: manual_establishment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT manual_establishment_pkey PRIMARY KEY (id);


--
-- Name: message_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_pkey PRIMARY KEY (id);


--
-- Name: payday_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY payday
    ADD CONSTRAINT payday_pkey PRIMARY KEY (id);


--
-- Name: pledge_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_pkey PRIMARY KEY (id);


--
-- Name: project_blog_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_pkey PRIMARY KEY (id);


--
-- Name: project_blog_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_pkey PRIMARY KEY (id);


--
-- Name: project_last_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_pkey PRIMARY KEY (id);


--
-- Name: project_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_pkey PRIMARY KEY (id);


--
-- Name: project_tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_pkey PRIMARY KEY (id);


--
-- Name: project_update_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_pkey PRIMARY KEY (id);


--
-- Name: project_user_role_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_pkey PRIMARY KEY (id);


--
-- Name: role_event_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_pkey PRIMARY KEY (id);


--
-- Name: tag_color_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_pkey PRIMARY KEY (id);


--
-- Name: tag_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT tag_pkey PRIMARY KEY (id);


--
-- Name: ticket_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_pkey PRIMARY KEY (id);


--
-- Name: transaction_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_pkey PRIMARY KEY (id);


--
-- Name: unique_comment_ancestor; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT unique_comment_ancestor UNIQUE (comment, ancestor);


--
-- Name: unique_comment_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT unique_comment_tag UNIQUE (comment, tag, "user");


--
-- Name: unique_committee_member; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT unique_committee_member UNIQUE ("user");


--
-- Name: unique_default_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT unique_default_tag UNIQUE (tag);


--
-- Name: unique_doc_name; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT unique_doc_name UNIQUE (name);


--
-- Name: unique_invite; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT unique_invite UNIQUE (code);


--
-- Name: unique_manual_establishment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY manual_establishment
    ADD CONSTRAINT unique_manual_establishment UNIQUE (established_user);


--
-- Name: unique_pledge; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT unique_pledge UNIQUE ("user", project);


--
-- Name: unique_project_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_account UNIQUE (account);


--
-- Name: unique_project_blog_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT unique_project_blog_comment UNIQUE (comment);


--
-- Name: unique_project_handle; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project
    ADD CONSTRAINT unique_project_handle UNIQUE (handle);


--
-- Name: unique_project_last_update; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT unique_project_last_update UNIQUE (project);


--
-- Name: unique_project_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT unique_project_tag UNIQUE (project, tag);


--
-- Name: unique_project_user_role; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT unique_project_user_role UNIQUE (project, "user", role);


--
-- Name: unique_tag; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag
    ADD CONSTRAINT unique_tag UNIQUE (name);


--
-- Name: unique_tag_color; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT unique_tag_color UNIQUE (tag, "user");


--
-- Name: unique_user; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user UNIQUE (ident);


--
-- Name: unique_user_account; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user_account UNIQUE (account);


--
-- Name: unique_view_time_user_project_type; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT unique_view_time_user_project_type UNIQUE ("user", project, type);


--
-- Name: unique_wiki_last_edit; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT unique_wiki_last_edit UNIQUE (page);


--
-- Name: unique_wiki_page_comment; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT unique_wiki_page_comment UNIQUE (comment);


--
-- Name: unique_wiki_target; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT unique_wiki_target UNIQUE (project, target);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: user_setting_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_pkey PRIMARY KEY (id);


--
-- Name: view_time_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_pkey PRIMARY KEY (id);


--
-- Name: volunteer_application_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_pkey PRIMARY KEY (id);


--
-- Name: volunteer_interest_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_pkey PRIMARY KEY (id);


--
-- Name: wiki_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_last_edit_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_comment_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_pkey PRIMARY KEY (id);


--
-- Name: wiki_page_pkey; Type: CONSTRAINT; Schema: public; Owner: snowdrift_test; Tablespace: 
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_pkey PRIMARY KEY (id);


--
-- Name: doc_event; Type: TRIGGER; Schema: public; Owner: snowdrift_test
--

CREATE TRIGGER doc_event AFTER INSERT OR DELETE ON doc FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();


--
-- Name: role_event; Type: TRIGGER; Schema: public; Owner: snowdrift_test
--

CREATE TRIGGER role_event AFTER INSERT OR DELETE ON project_user_role FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();


--
-- Name: comment_ancestor_ancestor_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_ancestor_fkey FOREIGN KEY (ancestor) REFERENCES comment(id);


--
-- Name: comment_ancestor_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_ancestor
    ADD CONSTRAINT comment_ancestor_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: comment_moderated_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_by_fkey FOREIGN KEY (moderated_by) REFERENCES "user"(id);


--
-- Name: comment_moderated_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_moderated_user_fkey FOREIGN KEY (moderated_by) REFERENCES "user"(id);


--
-- Name: comment_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_parent_fkey FOREIGN KEY (parent) REFERENCES comment(id);


--
-- Name: comment_rethread_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_rethread_moderator_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_moderator_fkey FOREIGN KEY (moderator) REFERENCES "user"(id);


--
-- Name: comment_rethread_new_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_new_discussion_fkey FOREIGN KEY (new_discussion) REFERENCES discussion(id);


--
-- Name: comment_rethread_new_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_new_parent_fkey FOREIGN KEY (new_parent) REFERENCES comment(id);


--
-- Name: comment_rethread_old_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_old_discussion_fkey FOREIGN KEY (old_discussion) REFERENCES discussion(id);


--
-- Name: comment_rethread_old_parent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_rethread
    ADD CONSTRAINT comment_rethread_old_parent_fkey FOREIGN KEY (old_parent) REFERENCES comment(id);


--
-- Name: comment_retraction_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_retraction
    ADD CONSTRAINT comment_retraction_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: comment_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: comment_tag_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment_tag
    ADD CONSTRAINT comment_tag_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: comment_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY comment
    ADD CONSTRAINT comment_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: committee_user_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: committee_user_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY committee_user
    ADD CONSTRAINT committee_user_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: default_tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY default_tag_color
    ADD CONSTRAINT default_tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: doc_current_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc
    ADD CONSTRAINT doc_current_version_fkey FOREIGN KEY (current_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_blessed_version_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_blessed_version_fkey FOREIGN KEY (blessed_version) REFERENCES wiki_edit(id);


--
-- Name: doc_event_doc_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY doc_event
    ADD CONSTRAINT doc_event_doc_fkey FOREIGN KEY (doc) REFERENCES doc(id);


--
-- Name: invite_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: invite_redeemed_by_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_redeemed_by_fkey FOREIGN KEY (redeemed_by) REFERENCES "user"(id);


--
-- Name: invite_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY invite
    ADD CONSTRAINT invite_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: message_from_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_from_fkey FOREIGN KEY ("from") REFERENCES "user"(id);


--
-- Name: message_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: message_to_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY message
    ADD CONSTRAINT message_to_fkey FOREIGN KEY ("to") REFERENCES "user"(id);


--
-- Name: pledge_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: pledge_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY pledge
    ADD CONSTRAINT pledge_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: project_blog_comment_blog_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_blog_fkey FOREIGN KEY (blog) REFERENCES project_blog(id);


--
-- Name: project_blog_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog_comment
    ADD CONSTRAINT project_blog_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: project_blog_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: project_blog_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_blog_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_blog
    ADD CONSTRAINT project_blog_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: project_last_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project
    ADD CONSTRAINT project_last_payday_fkey FOREIGN KEY (last_payday) REFERENCES payday(id);


--
-- Name: project_last_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_last_update_update_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_last_update
    ADD CONSTRAINT project_last_update_update_fkey FOREIGN KEY (update) REFERENCES project_update(id);


--
-- Name: project_tag_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_tag_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_tag
    ADD CONSTRAINT project_tag_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: project_update_author_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_author_fkey FOREIGN KEY (author) REFERENCES "user"(id);


--
-- Name: project_update_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_update
    ADD CONSTRAINT project_update_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: project_user_role_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY project_user_role
    ADD CONSTRAINT project_user_role_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: role_event_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: role_event_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY role_event
    ADD CONSTRAINT role_event_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: tag_color_tag_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_tag_fkey FOREIGN KEY (tag) REFERENCES tag(id);


--
-- Name: tag_color_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY tag_color
    ADD CONSTRAINT tag_color_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: ticket_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY ticket
    ADD CONSTRAINT ticket_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: transaction_credit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_credit_fkey FOREIGN KEY (credit) REFERENCES account(id);


--
-- Name: transaction_debit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_debit_fkey FOREIGN KEY (debit) REFERENCES account(id);


--
-- Name: transaction_payday_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY transaction
    ADD CONSTRAINT transaction_payday_fkey FOREIGN KEY (payday) REFERENCES payday(id);


--
-- Name: user_account_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_account_fkey FOREIGN KEY (account) REFERENCES account(id);


--
-- Name: user_setting_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY user_setting
    ADD CONSTRAINT user_setting_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: view_time_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: view_time_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY view_time
    ADD CONSTRAINT view_time_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_application_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_project_fkey FOREIGN KEY (project) REFERENCES project(id);


--
-- Name: volunteer_application_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_application
    ADD CONSTRAINT volunteer_application_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: volunteer_interest_interest_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_interest_fkey FOREIGN KEY (interest) REFERENCES interest(id);


--
-- Name: volunteer_interest_volunteer_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY volunteer_interest
    ADD CONSTRAINT volunteer_interest_volunteer_fkey FOREIGN KEY (volunteer) REFERENCES volunteer_application(id);


--
-- Name: wiki_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_edit_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_edit
    ADD CONSTRAINT wiki_edit_user_fkey FOREIGN KEY ("user") REFERENCES "user"(id);


--
-- Name: wiki_last_edit_edit_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_edit_fkey FOREIGN KEY (edit) REFERENCES wiki_edit(id);


--
-- Name: wiki_last_edit_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_last_edit
    ADD CONSTRAINT wiki_last_edit_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_comment_comment_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_comment_fkey FOREIGN KEY (comment) REFERENCES comment(id);


--
-- Name: wiki_page_comment_page_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page_comment
    ADD CONSTRAINT wiki_page_comment_page_fkey FOREIGN KEY (page) REFERENCES wiki_page(id);


--
-- Name: wiki_page_discussion_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
--

ALTER TABLE ONLY wiki_page
    ADD CONSTRAINT wiki_page_discussion_fkey FOREIGN KEY (discussion) REFERENCES discussion(id);


--
-- Name: wiki_page_project_fkey; Type: FK CONSTRAINT; Schema: public; Owner: snowdrift_test
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

