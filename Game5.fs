namespace MonoGame005

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System

type Sprite =
    {position: Vector2; speed: float32; texture: Texture2D; size: Point; offset: Point}
    member this.Draw(spriteBatch: SpriteBatch) =
        let sourceRect = Rectangle(this.offset, this.size)
        spriteBatch.Draw(this.texture, this.position, Nullable.op_Implicit sourceRect, Color.White)

module AnimationFrames =
    let horizontalStrip(frameCount, size: Point, offset: Point) =
        [| for i in 0..frameCount-1 ->
            Rectangle(offset + Point(size.X * i, 0) , size)
        |]

type Animation = 
    {   frames: Rectangle array
        fps: int
        currentFrame: int
        frameTimer: TimeSpan
        frameLength: TimeSpan
        size: Point }

    static member Create(frameCount, fps, size: Point, offset: Point) =
        let frames = AnimationFrames.horizontalStrip(frameCount, size, offset)
        {   frames = frames
            currentFrame = 0
            frameTimer = TimeSpan.Zero
            frameLength = TimeSpan.FromSeconds (float (1.f / float32 fps))
            fps = fps
            size = size }
    
    member this.CurrentFrame =
        this.frames.[this.currentFrame]

module Animation =
    let reset anim =
        {anim with
            currentFrame = 0
            frameTimer = TimeSpan.Zero }
        
    let update (gameTime: GameTime) (anim: Animation) =
        let newframeTimer, newFrame =
            match anim.frameTimer + gameTime.ElapsedGameTime with
            | n when n >= anim.frameLength ->
                TimeSpan.Zero, (anim.currentFrame + 1) % anim.frames.Length
            | n -> n, anim.currentFrame

        {anim with
            frameTimer = newframeTimer
            currentFrame = newFrame }

type AnimationKey =
    | IdleLeft
    | IdleRight
    | IdleDown
    | IdleUp
    | WalkLeft
    | WalkRight
    | WalkDown
    | WalkUp

type AnimatedSprite =
    {   texture: Texture2D
        animations: Map<AnimationKey, Animation>
        currentAnimationKey: AnimationKey
        isAnimating: bool
        speed: float32
        position: Vector2 }
    member this.CurrentAnimation = this.animations.[this.currentAnimationKey]
    member this.Size with get() = this.CurrentAnimation.size

module AnimatedSprite =  

    let resetAnimation key animatedSprite =
        animatedSprite.animations.[key]
        |> Animation.reset
        
    let updateAnimation key gameTime animatedSprite =
        let animation = animatedSprite.animations.[key]
        if animatedSprite.isAnimating then
            animation |> Animation.update gameTime
        else animation
        
    let draw (animSprite: AnimatedSprite) (gameTime: GameTime) (sb: SpriteBatch) =
        sb.Draw(animSprite.texture, animSprite.position, Nullable.op_Implicit animSprite.CurrentAnimation.CurrentFrame, Color.White)

[<AutoOpen>]
module MonoGameExtensions =
    type Viewport with
        member this.Center =
            Vector2(float32 this.Width * 0.5f, float32 this.Height * 0.5f)

type Camera(viewport: Viewport) =       
    member val WorldToScreen = Matrix.Identity with get, set
    member val ScreenToWorld = Matrix.Identity with get, set
    member val Zoom = 1.0f with get, set
    member val Position = Vector2.Zero with get, set
    member val Rotation = 0.0f with get, set

    member this.Update (pos:Vector2) =
        this.Position <- pos
        this.WorldToScreen <-
            Matrix.CreateTranslation(Vector3(-pos, 0.0f)) *
            Matrix.CreateRotationZ(this.Rotation ) *
            Matrix.CreateScale(Vector3(this.Zoom, this.Zoom, 1.f )) *
            Matrix.CreateTranslation(Vector3(viewport.Center, 0.f))
        this.ScreenToWorld <- Matrix.Invert(this.WorldToScreen)

type TileSet =
    { tilesWide: int
      tilesHigh: int
      tileWidth: int
      tileHeight: int
      texture:  Texture2D
      sourceRectangles: Rectangle array }

module TileSet =
  let createTileSet(tileswide, tileshigh,  tileheight, tilewidth, texture) =
      let sourceRectangles =
          [| for y in 0..tileshigh-1 do
               for x in 0..tileswide-1 do
                    yield Rectangle(x * tilewidth, y * tileheight, tilewidth, tileheight) |]

      { tilesWide = tileswide
        tilesHigh = tileshigh
        tileWidth = tilewidth
        tileHeight = tileheight
        texture = texture
        sourceRectangles = sourceRectangles }

type TileLayer = { tiles: int array
                   width: int
                   height: int
                   visible: bool }

module TileLayer =

    let getTileId x y (layer: TileLayer) =
        match x, y with
        | (x, y) when x < 0 || y < 0 -> None
        | (x ,y) when x > layer.width || y > layer.height ->
            None
        | _ ->
            let index = y * layer.width + x
            match layer.tiles |> Array.tryItem index with
            | Some tileId when tileId > 0 ->
                Some (tileId - 1) //id is one based, zero being an empty cell
            | _ -> None

    let vectorToCell (position: Vector2) (tileSet: TileSet) =
        Point( int position.X / tileSet.tileWidth, int position.Y / tileSet.tileHeight)
            
    let draw(spriteBatch: SpriteBatch, tileSet: TileSet, camera: Camera, layer : TileLayer, game: Game) =
        if not layer.visible then () else
        let cameraPoint =
            let location =
                Vector2(camera.Position.X - (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
                        camera.Position.Y - (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
            vectorToCell location tileSet 

        let viewPoint =
            let location =
                Vector2(camera.Position.X + (float32 game.GraphicsDevice.Viewport.Width * 0.5f),
                        camera.Position.Y + (float32 game.GraphicsDevice.Viewport.Height * 0.5f))
            vectorToCell location tileSet
        
        let minX, minY =  max 0 (cameraPoint.X - 1), max 0 (cameraPoint.Y - 1)
        let maxX, maxY =  min (viewPoint.X + 1) layer.width - 1, min (viewPoint.Y + 1) layer.height - 1

        for y in minY..maxY do
            for x in minX..maxX do
                match getTileId x y layer with
                | None  -> ()
                | Some tile ->
                    if tile = -1 then () else
                    let destination = Rectangle(x * tileSet.tileWidth, y * tileSet.tileHeight,
                                                tileSet.tileWidth, tileSet.tileHeight)
                    spriteBatch.Draw(tileSet.texture, destination, Nullable.op_Implicit tileSet.sourceRectangles.[tile], Color.White)

type Game5 () as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this, PreferredBackBufferWidth = 1920, PreferredBackBufferHeight = 1080)
    let mutable spriteBatch = Unchecked.defaultof<_>
    let mutable playerSpriteSheet = Unchecked.defaultof<Texture2D>
    let mutable player = Unchecked.defaultof<Sprite>
    let mutable newPlayer = Unchecked.defaultof<AnimatedSprite>
    let mutable playerAnimations = Unchecked.defaultof<_>
    let mutable camera = Unchecked.defaultof<_>
    let mutable tileSet = Unchecked.defaultof<TileSet>
    let mutable tileLayer = Unchecked.defaultof<TileLayer>
    let mutable terrain = Unchecked.defaultof<Texture2D>

    let (|KeyDown|_|) k (state: KeyboardState) =
        if state.IsKeyDown k then Some() else None

    let getMovementVector = function
        | KeyDown Keys.W & KeyDown Keys.A -> Vector2(-1.f, -1.f), WalkLeft
        | KeyDown Keys.W & KeyDown Keys.D -> Vector2(1.f, -1.f), WalkRight
        | KeyDown Keys.S & KeyDown Keys.A -> Vector2(-1.f, 1.f), WalkLeft
        | KeyDown Keys.S & KeyDown Keys.D -> Vector2(1.f, 1.f), WalkRight
        | KeyDown Keys.W -> Vector2(0.f, -1.f), WalkUp
        | KeyDown Keys.S -> Vector2(0.f, 1.f), WalkDown
        | KeyDown Keys.A -> Vector2(-1.f, 0.f), WalkLeft
        | KeyDown Keys.D -> Vector2(1.f, 0.f), WalkRight
        | _ -> Vector2.Zero, WalkDown

    do
        this.Content.RootDirectory <- "Content"
        this.IsMouseVisible <- false

    override this.Initialize() =
        let frameSize = Point(64, 64)
        let anims = 
            [   IdleUp,    Animation.Create(1, 1, frameSize, Point(0, 0))
                IdleLeft,  Animation.Create(1, 1, frameSize, Point(0, 64))
                IdleDown,  Animation.Create(1, 1, frameSize, Point(0, 128))
                IdleRight, Animation.Create(1, 1, frameSize, Point(0, 192))
                WalkUp,    Animation.Create(8, 10, frameSize, Point(64, 0))
                WalkLeft,  Animation.Create(8, 10, frameSize, Point(64, 64))
                WalkDown,  Animation.Create(8, 10, frameSize, Point(64, 128))
                WalkRight, Animation.Create(8, 10, frameSize, Point(64, 192)) ] |> Map.ofList
        playerAnimations <- anims
        camera <- Camera(this.GraphicsDevice.Viewport)
        terrain <- this.Content.Load<Texture2D>("terrain")
        tileSet <- TileSet.createTileSet(32, 32, 32, 32, terrain)
        let tiles =
            [|  190;189;190;188;190;188;189;189;29;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;157;30;189;188;188;188;190;189;190;189;188;190;188;189;190;189;
                188;189;189;189;189;190;29;157;158;257;258;258;259;0;0;0;65;66;66;66;66;66;66;66;66;66;66;66;66;66;66;66;67;156;65;67;156;157;30;189;190;188;125;125;125;190;189;189;
                125;190;29;157;157;157;158;257;258;230;290;357;229;261;261;262;129;3;98;163;163;163;161;98;163;162;98;98;98;161;162;98;34;66;35;34;66;67;156;157;157;157;157;157;157;30;188;190;
                188;29;158;65;66;67;260;230;290;356;354;355;355;356;356;291;0;97;98;98;98;601;98;98;98;601;601;605;98;98;98;98;163;2;130;130;130;131;257;261;258;262;65;66;67;156;30;188;
                189;126;65;35;98;99;292;353;353;290;357;354;358;290;355;291;0;97;98;98;604;98;505;506;506;506;506;506;507;98;98;98;2;131;257;258;258;258;227;290;293;291;129;3;34;67;124;188;
                190;126;97;98;98;99;292;354;355;355;290;356;357;354;290;291;0;97;163;163;604;601;537;538;538;538;538;538;539;98;163;98;99;260;227;644;645;645;646;353;356;229;262;97;162;99;124;188;
                189;126;97;98;162;99;324;195;354;290;354;354;290;355;293;291;0;35;98;98;161;603;569;570;570;570;509;538;540;507;161;98;99;292;644;614;677;677;613;645;645;646;291;97;2;131;124;188;
                190;126;129;130;3;34;67;289;355;356;293;293;354;358;290;291;65;2;130;3;605;603;601;604;604;605;537;538;538;540;507;98;99;292;676;677;677;677;677;677;677;678;294;97;34;67;124;189;
                189;61;93;94;97;162;99;292;358;357;357;355;353;354;355;294;97;99;0;129;3;604;602;603;601;601;569;509;538;538;539;161;99;289;708;709;709;582;677;677;677;678;294;97;98;99;124;189;
                190;188;188;126;97;163;99;289;358;293;353;353;353;357;293;291;129;131;275;277;129;130;130;130;130;130;3;569;570;570;571;98;99;321;325;322;195;708;709;709;709;710;294;97;98;99;124;188;
                189;190;188;126;97;163;99;292;353;358;356;354;354;194;322;326;0;275;245;244;276;276;276;276;277;0;97;603;2;130;130;3;34;66;66;67;324;322;322;325;322;322;323;97;2;131;124;189;
                190;189;190;126;97;98;99;321;325;325;325;325;322;323;65;66;67;339;213;371;373;373;308;212;341;65;35;98;99;80;82;129;130;3;161;34;66;66;66;66;66;66;66;35;99;92;62;189;
                188;190;188;126;97;161;34;66;66;66;66;66;66;66;35;98;99;0;307;373;373;308;308;244;277;129;130;130;131;112;49;81;82;129;3;161;163;2;130;130;130;3;98;2;131;124;190;189;
                189;189;189;126;97;163;98;98;98;162;2;130;130;130;3;98;34;67;339;340;340;340;340;340;341;77;79;80;81;50;176;177;114;65;35;98;161;99;272;273;274;129;3;34;67;156;30;190;
                190;190;188;126;129;3;161;98;98;98;99;269;270;271;129;3;98;99;65;66;66;66;67;0;77;47;111;144;18;176;113;178;114;97;98;2;3;99;304;305;241;274;97;163;34;67;156;30;
                189;190;189;61;94;129;3;98;98;98;99;301;302;238;271;129;130;3;35;98;98;98;34;67;109;110;46;79;144;18;113;178;114;97;98;34;35;99;304;305;305;306;97;163;98;34;67;124;
                188;189;188;188;61;94;97;162;98;98;99;301;302;302;238;271;0;129;130;130;3;98;98;99;109;110;173;46;79;144;18;176;114;129;3;98;2;131;304;305;305;306;129;3;163;161;99;124;
                188;190;188;189;189;126;129;3;98;98;99;333;207;302;302;238;270;270;270;271;129;130;3;99;141;142;142;15;46;79;112;176;49;82;97;98;34;67;336;210;305;306;65;35;98;98;99;124;
                190;190;189;190;189;61;94;129;3;98;34;67;333;207;302;302;302;302;302;238;270;271;129;131;65;67;0;141;15;111;144;18;113;114;129;3;98;34;67;304;305;306;129;3;98;2;131;124;
                188;188;190;189;190;190;61;94;97;98;98;99;0;333;207;302;302;302;302;302;302;238;270;271;97;34;66;67;109;46;79;112;176;49;82;129;3;98;99;336;210;241;274;97;163;99;92;62;
                188;188;189;189;189;188;188;126;97;98;98;34;67;0;301;302;302;302;302;302;302;302;302;303;97;98;2;131;109;175;111;112;113;113;49;82;129;3;34;67;304;305;306;129;3;99;124;190;
                188;189;189;29;157;157;157;158;97;161;605;98;34;67;333;334;334;207;302;302;302;302;206;335;97;98;34;67;109;174;111;112;113;177;177;114;0;129;3;99;336;210;241;274;97;99;124;188;
                189;189;190;126;65;66;66;66;35;98;163;98;98;34;66;66;67;333;334;334;334;207;303;65;35;161;98;99;141;15;111;144;145;18;178;49;82;65;35;34;67;304;305;306;97;99;124;189;
                189;188;189;126;129;3;163;98;161;162;161;163;162;98;163;98;34;66;66;66;67;333;335;97;98;163;98;99;0;109;46;78;79;112;178;176;114;97;98;98;99;336;337;338;97;99;124;188;
                190;190;190;61;94;97;98;163;162;163;163;161;98;161;98;98;98;98;98;98;34;66;66;35;163;161;98;99;77;47;174;174;111;144;18;113;114;97;98;98;34;66;66;66;35;99;124;188;
                189;188;190;188;126;97;602;162;98;98;98;98;98;98;98;98;98;2;3;98;98;98;98;98;98;98;98;99;109;174;174;174;46;79;144;145;146;97;161;98;161;163;98;672;98;99;124;190;
                189;189;188;29;158;97;98;98;98;161;163;161;98;162;161;98;161;99;129;130;130;3;161;98;98;98;163;99;141;142;15;174;14;143;65;66;66;35;98;163;98;98;98;704;98;99;124;190;
                188;189;29;158;65;35;162;601;98;161;163;162;98;98;98;161;98;99;92;93;94;129;130;130;163;98;98;34;66;67;141;142;143;65;35;163;163;2;130;130;130;3;98;98;98;99;124;189;
                190;188;126;65;35;98;163;163;98;161;162;162;161;2;130;130;130;131;124;188;61;93;93;94;129;130;3;98;98;34;66;67;65;35;98;98;162;99;525;461;526;97;98;98;98;99;124;190;
                189;188;126;97;162;162;162;162;98;162;98;98;98;99;92;93;93;93;62;188;189;189;189;61;93;94;97;161;98;98;98;34;35;98;163;98;98;99;430;399;428;97;161;162;98;99;124;189;
                189;190;126;97;98;161;161;161;162;98;162;161;98;99;124;189;189;189;188;188;188;190;190;189;29;158;97;98;98;98;98;98;98;98;161;98;2;131;430;399;428;97;163;98;2;131;124;188;
                189;188;126;97;163;162;161;162;161;162;163;98;163;99;124;190;190;190;190;189;190;189;214;189;126;65;35;98;98;163;161;98;98;161;98;2;131;525;462;463;428;97;161;98;99;92;62;189;
                188;188;126;97;98;162;162;161;161;162;162;161;2;131;156;157;30;189;190;190;246;189;190;190;126;97;98;98;98;98;98;98;161;98;2;131;525;462;463;495;428;97;98;161;99;124;189;190;
                190;189;126;97;163;161;162;163;98;161;163;98;34;66;66;67;124;188;189;189;189;189;189;190;126;97;98;98;162;163;98;98;2;130;131;525;462;492;399;396;558;97;98;98;99;124;189;188;
                190;188;126;129;130;130;3;162;161;163;163;161;163;161;163;99;124;190;189;189;190;190;189;189;126;97;98;98;98;163;98;2;131;525;461;462;493;492;396;558;65;35;98;163;99;156;30;189;
                190;189;61;93;93;94;129;130;130;130;130;130;130;130;3;99;156;30;189;189;189;189;190;190;126;97;98;98;98;98;161;99;525;462;493;492;494;431;428;65;35;98;162;98;34;67;124;189;
                189;190;190;190;190;61;93;93;93;93;93;93;93;94;129;131;92;62;189;190;188;190;189;190;126;97;98;98;98;98;98;99;557;397;397;397;397;397;558;97;163;98;98;163;162;99;124;190;
                188;190;188;188;189;190;188;188;189;190;188;125;125;61;93;93;62;188;190;188;188;189;189;188;126;97;98;98;603;602;603;34;66;66;66;66;66;66;66;35;98;98;162;98;98;99;124;189;
                189;189;189;189;190;188;188;189;189;189;188;190;188;190;190;188;188;190;188;190;188;190;188;189;126;129;3;161;572;605;604;98;163;602;98;604;602;601;605;602;2;130;130;130;130;131;124;189;
                188;190;190;189;190;190;188;189;189;189;190;188;189;190;190;190;188;190;189;190;188;188;188;190;61;94;129;130;130;130;130;130;130;130;130;130;130;130;130;130;131;92;93;93;93;93;62;189;
                190;189;189;188;188;188;190;190;188;190;188;190;190;188;190;190;189;190;189;189;188;190;188;189;188;61;93;93;93;93;93;93;93;93;93;93;93;93;93;93;93;62;189;188;189;190;189;189;
                188;190;188;190;189;190;190;189;189;189;188;190;188;190;188;188;190;376;190;189;188;189;374;189;189;188;188;189;190;189;188;188;190;189;190;188;189;188;188;189;188;190;189;189;190;189;189;188
        |]

        tileLayer <- { tiles = tiles
                       width = 48
                       height = 42
                       visible = true }
        base.Initialize()

    override this.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        playerSpriteSheet <- this.Content.Load<Texture2D>("skeleton")
        player <- { position = Vector2.Zero
                    speed= 166.f
                    texture = playerSpriteSheet
                    size = Point(64, 64)
                    offset = Point(0,128) }

        newPlayer <- {  texture = playerSpriteSheet
                        animations = playerAnimations
                        currentAnimationKey = AnimationKey.IdleDown
                        isAnimating = false
                        speed = 166.f
                        position = Vector2( float32 (tileLayer.width * tileSet.tileWidth) / 2.f, float32 (tileLayer.height * tileSet.tileHeight) / 2.f) }

    override this.Update (gameTime) =
        if (GamePad.GetState(PlayerIndex.One).Buttons.Back = ButtonState.Pressed || Keyboard.GetState().IsKeyDown(Keys.Escape))
        then this.Exit()

        let walkingToIdle = function
            | WalkUp -> IdleUp
            | WalkLeft -> IdleLeft
            | WalkDown -> IdleDown
            | WalkRight -> IdleRight
            | otherIdle -> otherIdle

        let movementVector, isMoving, animationKey =
            let movementVector, animationKey = getMovementVector(Keyboard.GetState())
            if movementVector = Vector2.Zero then
                movementVector, false, walkingToIdle newPlayer.currentAnimationKey
            else movementVector |> Vector2.Normalize, true, animationKey
            
        let newPosition player =
            let newPos =
                player.position + movementVector * player.speed * float32 gameTime.ElapsedGameTime.TotalSeconds
            
            let minClamp = Vector2.Zero
            
            let maxClamp =
                Vector2(float32 (tileLayer.width * tileSet.tileWidth) - float32 player.Size.X,
                        float32 (tileLayer.height * tileSet.tileHeight) - float32 player.Size.Y)
            
            Vector2.Clamp(newPos, minClamp, maxClamp)      

        let newAnimation =
            if newPlayer.currentAnimationKey = animationKey then
                newPlayer |> AnimatedSprite.updateAnimation animationKey gameTime
            else
                newPlayer |> AnimatedSprite.resetAnimation animationKey

        newPlayer <- { newPlayer with
                        position = newPosition newPlayer
                        isAnimating = isMoving
                        currentAnimationKey = animationKey
                        animations = newPlayer.animations |> Map.add animationKey newAnimation }

        camera.Update newPlayer.position

        base.Update(gameTime)

    override this.Draw (gameTime) =
        this.GraphicsDevice.Clear Color.CornflowerBlue
        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp, transformMatrix = Nullable.op_Implicit camera.WorldToScreen)
        TileLayer.draw(spriteBatch, tileSet, camera, tileLayer, this)
        player.Draw(spriteBatch)
        AnimatedSprite.draw newPlayer gameTime spriteBatch
        spriteBatch.End()
        base.Draw(gameTime)